;;; teletext.el --- Teletext broadcast viewer -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: ISC
;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/emacs-teletext
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))
;; Package-Version: 0.1.0
;; Keywords: comm help hypermedia
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Browse teletext pages in Emacs.
;;
;; You need to install one or more teletext providers in addition to
;; this package. Look for package names of the form `teletext-*'.
;;
;;; Code:

;; History: There was an earlier videotext package for Emacs, vtx.el
;; written in 1998 by Lars Magne Ingebrigtsen. The present package
;; shares no code or functionality with vtx.el but I have kept the
;; same key bindings.

(defvar teletext-providers '())

(defvar-local teletext-state nil)

(defun teletext--clamp-page (page)
  (cond ((not (integerp page)) 100)
        ((< page 100) 100)
        ((> page 899) 899)
        (t page)))

(defun teletext--get-state (key)
  (cdr (assoc key teletext-state)))

(defun teletext--set-state (key value)
  (let ((apair (or (assoc key teletext-state)
                   (car (push (list key) teletext-state)))))
    (setcdr apair value)
    value))

(defun teletext--merge-state (new-state)
  (dolist (new-apair new-state teletext-state)
    (let ((apair (assoc (car new-apair) teletext-state)))
      (if apair (setcdr apair (cdr new-apair))
        (push new-apair teletext-state)))))

(defun teletext--update (thunk)
  (cl-assert (equal major-mode 'teletext-mode))
  (cl-assert (null (buffer-file-name)))
  (funcall thunk))

(defun teletext--format-time (time)
  (let ((formatted (teletext--get-state 'network-time-format)))
    (when formatted
      (let* ((decoded (decode-time time))
             (month (nth 4 decoded))
             (day (nth 3 decoded))
             (hour (nth 2 decoded))
             (minute (nth 1 decoded))
             (substitutions `(("{dd}" . ,(format "%02d" day))
                              ("{mm}" . ,(format "%02d" month))
                              ("{HH}" . ,(format "%02d" hour))
                              ("{MM}" . ,(format "%02d" minute)))))
        (dolist (subst substitutions formatted)
          (setq formatted
                (replace-regexp-in-string (regexp-quote (car subst))
                                          (cdr subst)
                                          formatted)))))))

(defun teletext--revert-header-line ()
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (delete-region (point-at-bol) (point-at-eol))
    (insert
     (format
      "  %3d  %d/%d    %s   %s   %s %s"
      (teletext--get-state 'page)
      (teletext--get-state 'subpage)
      (teletext--get-state 'subpages)
      (teletext--get-state 'network-heading)
      (teletext--format-time (current-time))
      (or (teletext--get-state 'network-page-text) "Page:")
      (let ((input (or (teletext--get-state 'input) 0)))
        (if (= input 0) "" (format "%s" input)))))))

(defun teletext--revert-header-line-only ()
  (teletext--update #'teletext--revert-header-line))

(defun teletext--revert (&optional _arg _noconfirm)
  (teletext--update
   (lambda ()
     (let ((inhibit-read-only t))
       (teletext--set-state
        'page (teletext--clamp-page (teletext--get-state 'page)))
       (erase-buffer)
       (insert "\n")
       (let ((provider-symbol (teletext--get-state 'provider))
             (network (teletext--get-state 'network)))
         (cond ((and provider-symbol network)
                (let* ((provider (assoc provider-symbol teletext-providers))
                       (provider-page-fn (cdr (assoc 'page provider)))
                       (page (teletext--get-state 'page))
                       (subpage (teletext--get-state 'subpage)))
                  (teletext--merge-state
                   (funcall provider-page-fn network page subpage))
                  (teletext--revert-header-line)))
               ((null teletext-providers)
                (insert "\nNo networks. Install a provider."))
               (t
                (insert "\nNetworks:\n\n")
                (dolist (provider teletext-providers)
                  (let* ((provider-networks-fn
                          (cdr (assoc 'networks provider)))
                         (networks (funcall provider-networks-fn)))
                    (dolist (network networks)
                      (insert (format "%s\n" network))))))))))))

(defun teletext--network-list ()
  (let ((all-networks '()))
    (dolist (provider teletext-providers all-networks)
      (let* ((provider-networks-fn (cdr (assoc 'networks provider)))
             (networks (funcall provider-networks-fn)))
        (setq all-networks (nconc all-networks networks))))))

(defun teletext-select-network (network)
  (interactive
   (list (let ((networks (teletext--network-list)))
           (completing-read "Teletext network: " networks
                            nil t nil nil (first networks)))))
  (cl-dolist (provider teletext-providers
                       (error "No such teletext network: %S" network))
    (let* ((provider-symbol (car provider))
           (provider-networks-fn (cdr (assoc 'networks (cdr provider))))
           (networks (funcall provider-networks-fn)))
      (when (member network networks)
        (teletext--set-state 'provider provider-symbol)
        (teletext--set-state 'network network)
        (message "Switched to teletext network %s" network)
        (teletext-goto-page 100)
        (return network)))))

(defun teletext-goto-page (page)
  (teletext--set-state 'input nil)
  (let ((page (teletext--clamp-page page)))
    (unless (equal (teletext--get-state 'page) page)
      (teletext--set-state 'page page)
      (teletext--set-state 'subpage 1)
      (teletext--revert))
    page))

(defun teletext-previous-page ()
  (interactive)
  (teletext-goto-page (1- (or (teletext--get-state 'page) 100))))

(defun teletext-next-page ()
  (interactive)
  (teletext-goto-page (1+ (or (teletext--get-state 'page) 100))))

(defun teletext-previous-subpage ()
  (interactive))

(defun teletext-next-subpage ()
  (interactive))

(defun teletext--input-changed ()
  (teletext--revert-header-line)
  (let ((input (teletext--get-state 'input)))
    (when (>= input 100)
      (teletext-goto-page input))))

(defun teletext-input-digit (char)
  (interactive (list last-command-event))
  (cl-assert (and (characterp char) (<= ?0 char ?9)))
  (let ((digit (- char ?0))
        (input (teletext--get-state 'input)))
    (teletext--set-state 'input (+ digit (* 10 (or input 0))))
    (teletext--input-changed)))

(defun teletext-input-backspace ()
  (interactive)
  (let ((input (teletext--get-state 'input)))
    (teletext--set-state 'input (truncate (or input 0) 10))
    (teletext--input-changed)))

(defvar teletext-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map [backspace] 'teletext-input-backspace)
    (define-key map [down] 'teletext-previous-page)
    (define-key map [up] 'teletext-next-page)
    (define-key map [left] 'teletext-previous-subpage)
    (define-key map [right] 'teletext-next-subpage)
    (define-key map (kbd "b") 'teletext-previous-subpage)
    (define-key map (kbd "f") 'teletext-next-subpage)
    (define-key map (kbd "n") 'teletext-next-page)
    (define-key map (kbd "p") 'teletext-previous-page)
    (define-key map (kbd "s") 'teletext-select-network)
    (dotimes (i 10)
      (define-key map (kbd (format "%d" i)) 'teletext-input-digit))
    map)
  "Keymap for `teletext-mode'.")

(define-derived-mode teletext-mode special-mode "Teletext"
  "Major mode for browsing Teletext pages.

\\{teletext-mode-map}"
  (setq-local revert-buffer-function 'teletext--revert)
  (teletext--revert))

(cl-defun teletext-provide (symbol &key networks page)
  (cl-assert (functionp networks))
  (cl-assert (functionp page))
  (let ((apair (or (assoc symbol teletext-providers)
                   (car (push (list symbol) teletext-providers)))))
    (setcdr apair (list (cons 'networks networks) (cons 'page page)))
    symbol))

;;;###autoload
(defun teletext ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*Teletext*"))
  (unless (equal major-mode 'teletext-mode)
    (teletext-mode)))

(provide 'teletext)

;;; teletext.el ends here
