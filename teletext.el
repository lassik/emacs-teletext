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
;; this package.  Look for package names of the form `teletext-*'.
;;
;;; Code:

;; History: There was an earlier videotext package for Emacs, vtx.el
;; written in 1998 by Lars Magne Ingebrigtsen.  The present package
;; shares no code or functionality with vtx.el but I have kept the
;; same key bindings.

(defvar teletext--providers '()
  "List of Emacs packages providing teletext service.

The package `teletext' does not actually provide a teletext
broadcast.  Broadcasts are provided by third-party packages.

Each such package calls `teletext-provide' internally to register
itself with the `teletext' package.")

(defvar-local teletext--state nil
  "Internal variable remembering where you're at in a teletext buffer.")

(defun teletext--get-face (background foreground)
  "Helper for programmers who make new teletext providers.

Returns a face for displaying the given combination of BACKGROUND
and FOREGROUND colors.  They are strings like \"black\" and
\"green\"."
  (let* ((name (concat "teletext--face-" background "-" foreground))
         (face (make-face (intern name))))
    (set-face-background face background)
    (set-face-foreground face foreground)
    face))

(defun teletext-put-color (background foreground start end)
  "Helper for programmers who make new teletext providers.

Colors the region of text from START to END in the current buffer
using the given BACKGROUND and FOREGROUND color.  If FOREGROUND
is nil, BACKGROUND is used for the foreground color as well.

A color is one of the lowercase strings \"black\", \"red\",
\"green\", \"yellow\", \"blue\", \"magenta\", \"cyan\", \"white\"."
  (let ((overlay (make-overlay start end))
        (face (teletext--get-face background (or foreground background))))
    (overlay-put overlay 'face face)
    overlay))

(defun teletext-insert-spaces (background count)
  "Helper for programmers who make new teletext providers.

Insert COUNT invisible space characters at point and move point
to the end of the spaces. The given BACKGROUND color is used.  If
COUNT is negative or zero, nothing is inserted."
  (when (> count 0)
    (let ((start (point)))
      (insert (make-string count ? ))
      (teletext-put-color background nil start (point)))))

(defun teletext--clamp-page (page)
  "Internal helper to ensure PAGE is between 100..899."
  (cond ((not (integerp page)) 100)
        ((< page 100) 100)
        ((> page 899) 899)
        (t page)))

(defun teletext--clamp-subpage (subpage count)
  "Internal helper to ensure SUBPAGE is between 1..COUNT."
  (let ((count (or count 1)))
    (cond ((not (integerp subpage)) 1)
          ((< subpage 1) count)
          ((> subpage count) 1)
          (t subpage))))

(defun teletext--get-state (key)
  "Internal helper to get data KEY for the current buffer."
  (cdr (assoc key teletext--state)))

(defun teletext--set-state (key value)
  "Internal helper to set data KEY to VALUE for the current buffer."
  (let ((apair (or (assoc key teletext--state)
                   (car (push (list key) teletext--state)))))
    (setcdr apair value)
    value))

(defun teletext--merge-state (new-state)
  "Internal helper to set NEW-STATE data for the current buffer."
  (dolist (new-apair new-state teletext--state)
    (let ((apair (assoc (car new-apair) teletext--state)))
      (if apair (setcdr apair (cdr new-apair))
        (push new-apair teletext--state)))))

(defun teletext--update (thunk)
  "Internal helper to refresh a teletext page using function THUNK."
  (cl-assert (equal major-mode 'teletext-mode))
  (cl-assert (null (buffer-file-name)))
  (funcall thunk))

(defun teletext--format-time (time)
  "Internal helper to display TIME on the teletext screen."
  (let ((formatted (teletext--get-state 'network-time-format)))
    (when formatted
      (let* ((decoded (decode-time time))
             (month (nth 4 decoded))
             (day (nth 3 decoded))
             (hour (nth 2 decoded))
             (minute (nth 1 decoded))
             (case-fold-search nil)
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
  "Internal helper for the status line at the top of the teletext screen."
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (delete-region (point-at-bol) (point-at-eol))
    (let* ((input (or (teletext--get-state 'input) 0))
           (blank-input-p (= input 0))
           (left-justified-part
            (format "  %3d %2d/%-2d %s"
                    (teletext--get-state 'page)
                    (teletext--get-state 'subpage)
                    (teletext--get-state 'subpages)
                    (teletext--get-state 'network-heading)))
           (right-justified-part
            (if blank-input-p
                (teletext--format-time (current-time))
                (or (teletext--get-state 'network-page-text) "Page:")))
           (tail (if blank-input-p "" (format " %d" input)))
           (tail-max-length (if blank-input-p 0 4))
           (gap-length (max 1 (- 40 tail-max-length
                                 (length left-justified-part)
                                 (length right-justified-part))))
           (gap (make-string gap-length ? ))
           (line (concat left-justified-part gap right-justified-part tail)))
      (insert line))))

(defun teletext--revert-header-line-only ()
  "Internal helper to refresh a teletext page."
  (teletext--update #'teletext--revert-header-line))

(defun teletext--revert (&optional _arg _noconfirm)
  "Internal helper to refresh a teletext page."
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
                (let* ((provider (assoc provider-symbol teletext--providers))
                       (provider-page-fn (cdr (assoc 'page provider)))
                       (page (teletext--get-state 'page))
                       (subpage (teletext--get-state 'subpage)))
                  (teletext--merge-state
                   (funcall provider-page-fn network page subpage))
                  (teletext--revert-header-line)))
               ((null teletext--providers)
                (insert "\nNo networks. Install a teletext provider."))
               (t
                (insert "\nNetworks:\n\n")
                (dolist (provider teletext--providers)
                  (let* ((provider-networks-fn
                          (cdr (assoc 'networks provider)))
                         (networks (funcall provider-networks-fn)))
                    (dolist (network networks)
                      (insert (format "%s\n" network))))))))))))

(defun teletext--network-list ()
  "Internal helper to get a list of all provided teletext networks."
  (let ((all-networks '()))
    (dolist (provider teletext--providers all-networks)
      (let* ((provider-networks-fn (cdr (assoc 'networks provider)))
             (networks (funcall provider-networks-fn)))
        (setq all-networks (nconc all-networks networks))))))

(defun teletext-select-network (network)
  "Change to a particular teletext network.

When called interactively, asks for the network in the minibuffer
with tab completion.  Otherwise, NETWORK is a string giving the
name of the network."
  (interactive
   (list (let ((networks (teletext--network-list)))
           (completing-read "Teletext network: " networks
                            nil t nil nil (car networks)))))
  (cl-dolist (provider teletext--providers
                       (error "No such teletext network: %S" network))
    (let* ((provider-symbol (car provider))
           (provider-networks-fn (cdr (assoc 'networks (cdr provider))))
           (networks (funcall provider-networks-fn)))
      (when (member network networks)
        (teletext--set-state 'provider provider-symbol)
        (teletext--set-state 'network network)
        (message "Switched to teletext network %s" network)
        (teletext-goto-page 100)
        (cl-return network)))))

(defun teletext-goto-page (page)
  "Change the teletext display to the given PAGE number (100..899)."
  (teletext--set-state 'input nil)
  (let ((page (teletext--clamp-page page)))
    (teletext--set-state 'page page)
    (teletext--set-state 'subpage 1)
    (teletext--revert)
    page))

(defun teletext-goto-subpage (subpage)
  "Change the teletext display to the given SUBPAGE of the current page."
  (teletext--set-state 'input nil)
  (let* ((count (teletext--get-state 'subpages))
         (subpage (teletext--clamp-subpage subpage count)))
    (teletext--set-state 'subpage subpage)
    (teletext--revert)
    subpage))

(defun teletext-previous-page ()
  "Change the teletext display to the previous PAGE."
  (interactive)
  (teletext-goto-page (or (teletext--get-state 'prev-page)
                          (1- (or (teletext--get-state 'page) 100)))))

(defun teletext-next-page ()
  "Change the teletext display to the next PAGE."
  (interactive)
  (teletext-goto-page (or (teletext--get-state 'next-page)
                          (1+ (or (teletext--get-state 'page) 100)))))

(defun teletext-previous-subpage ()
  "Change the teletext display to the previous SUBPAGE of the current page."
  (interactive)
  (teletext-goto-subpage (1- (or (teletext--get-state 'subpage) 1))))

(defun teletext-next-subpage ()
  "Change the teletext display to the next SUBPAGE of the current page."
  (interactive)
  (teletext-goto-subpage (1+ (or (teletext--get-state 'subpage) 1))))

(defun teletext--input-changed ()
  "Internal helper to handle teletext page number input."
  (teletext--revert-header-line)
  (let ((input (teletext--get-state 'input)))
    (when (>= input 100)
      (teletext-goto-page input))))

(defun teletext-input-digit (char)
  "Add the given digit CHAR to the teletext page number input field.

If this is the third digit, the page number input is completed
and the display changes to that page."
  (interactive (list last-command-event))
  (cl-assert (and (characterp char) (<= ?0 char ?9)))
  (let ((digit (- char ?0))
        (input (teletext--get-state 'input)))
    (teletext--set-state 'input (+ digit (* 10 (or input 0))))
    (teletext--input-changed)))

(defun teletext-input-backspace ()
  "Erase the last digit from the teletext page number input field."
  (interactive)
  (let ((input (teletext--get-state 'input)))
    (teletext--set-state 'input (truncate (or input 0) 10))
    (teletext--input-changed)))

(defvar teletext-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "b") 'teletext-previous-subpage)
    (define-key map (kbd "f") 'teletext-next-subpage)
    (define-key map (kbd "n") 'teletext-next-page)
    (define-key map (kbd "p") 'teletext-previous-page)
    (define-key map (kbd "s") 'teletext-select-network)
    (define-key map [backspace] 'teletext-input-backspace)
    (define-key map [deletechar] 'teletext-input-backspace)
    (define-key map [down] 'teletext-previous-page)
    (define-key map [left] 'teletext-previous-subpage)
    (define-key map [right] 'teletext-next-subpage)
    (define-key map [up] 'teletext-next-page)
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
  "Helper for programmers who make new teletext providers."
  (cl-assert (functionp networks))
  (cl-assert (functionp page))
  (let ((apair (or (assoc symbol teletext--providers)
                   (car (push (list symbol) teletext--providers)))))
    (setcdr apair (list (cons 'networks networks) (cons 'page page)))
    symbol))

;;;###autoload
(defun teletext ()
  "Browse teletext pages in Emacs."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Teletext*"))
  (unless (equal major-mode 'teletext-mode)
    (teletext-mode)))

(provide 'teletext)

;;; teletext.el ends here
