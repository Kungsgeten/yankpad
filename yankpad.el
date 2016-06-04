
;;; yankpad.el --- Paste snippets from an org-mode file

;; Copyright (C) 2016 Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand
;; URL: http://github.com/Kungsgeten/yankpad
;; Version: 1.10
;; Keywords: abbrev convenience
;; Package-Requires: ()

;;; Commentary:

;; A way to insert text snippets from an org-mode file.  The org-mode file in
;; question is defined in `yankpad-file' and is set to "yankpad.org" in your
;; `org-directory' by default.  In this file, each heading specifies a snippet
;; category and each subheading of that category defines a snippet.  This way
;; you can have different yankpads for different occasions.
;;
;; If you have yasnippet installed, yankpad will try to use it when pasting
;; snippets.  This means that you can use the features that yasnippet provides
;; (tab stops, elisp, etc).  You can use yankpad without yasnippet, and then the
;; snippet will simply be inserted as is.
;;
;; If you name a category to a major-mode name, that category will be switched
;; to when you change major-mode.  If you have projectile installed, you can also
;; name a categories to the same name as your projecile projects, and they will
;; be switched to when using `projectile-find-file'.
;;
;; To insert a snippet from the yankpad, use `yankpad-insert'.  If you need to
;; change the category, use `yankpad-set-category'.  Here's an example of what
;; yankpad.org could look like:

;;; Yankpad example:

;; ** Snippet 1
;; 
;;    This is a snippet.
;; 
;; ** Snippet 2
;;
;;    This is another snippet
;;    \* Org-mode doesn't like lines beginning with *
;;    Typing \* at the beginning of a line will be replaced with *
;; 
;; * Category 2
;; ** Snippet 1
;;
;;    This is yet another snippet, in a different category.

;;; Code:

(require 'org-element)

(defvar yankpad-file (expand-file-name "yankpad.org" org-directory)
  "The path to your yankpad.")

(defvar yankpad-category nil
  "The current yankpad category.  Change with `yankpad-set-category'.")
(put 'yankpad-category 'safe-local-variable #'stringp)

(defvar yankpad-category-heading-level 1
  "The org-mode heading level of categories in the `yankpad-file'.")

(defvar yankpad-snippet-heading-level 2
  "The org-mode heading level of snippets in the `yankpad-file'.")

(defvar yankpad-switched-category-hook nil
  "Hooks run after changing `yankpad-category'.")

(define-prefix-command 'yankpad-map)

(defun yankpad-set-category ()
  "Change the yankpad category."
  (interactive)
  (setq yankpad-category
        (completing-read "Category: " (yankpad--categories)))
  (run-hooks 'yankpad-switched-category-hook))

(defun yankpad-set-local-category (category)
  "Set `yankpad-category' to CATEGORY locally."
  (set (make-local-variable 'yankpad-category) category)
  (run-hooks 'yankpad-switched-category-hook))

;;;###autoload
(defun yankpad-insert ()
  "Insert an entry from the yankpad.
Uses `yankpad-category', and prompts for it if it isn't set."
  (interactive)
  (unless yankpad-category
    (or (yankpad-local-category-to-major-mode)
        (yankpad-set-category)))
  (yankpad-insert-from-category yankpad-category))

(defun yankpad--insert-snippet-text (text)
  "Inserts TEXT into buffer.
Uses yasnippet if that is installed."
  (if (and (require 'yasnippet nil t)
           yas-minor-mode)
      (if (region-active-p)
          (yas-expand-snippet text (region-beginning) (region-end))
        (yas-expand-snippet text))
    (insert text)))

(defun yankpad-insert-from-category (category)
  "Insert a yankpad entry from CATEGORY.
Does not change `yankpad-category'."
  (let* ((snippets (yankpad--snippets category))
         (snippet (completing-read "Snippet: " snippets))
         (text (replace-regexp-in-string
                "^\\\\[*]" "*" (cadr (assoc snippet snippets)))))
    (yankpad--insert-snippet-text text)))

(defun yankpad-edit ()
  "Open the yankpad file for editing."
  (interactive)
  (find-file yankpad-file))

(defun yankpad--file-elements ()
  "Run `org-element-parse-buffer' on the `yankpad-file'."
  (with-temp-buffer
    (delay-mode-hooks
      (org-mode)
      (insert-file-contents yankpad-file)
      (org-element-parse-buffer))))

(defun yankpad--categories ()
  "Get the yankpad categories as a list."
  (let ((data (yankpad--file-elements)))
    (org-element-map data 'headline
      (lambda (h)
        (when (equal (org-element-property :level h)
                     yankpad-category-heading-level)
          (org-element-property :raw-value h))))))

(defun yankpad--snippet-elements (category-name)
  "Get all the snippet org-mode heading elements in CATEGORY-NAME."
  (let ((data (yankpad--file-elements)))
    (org-element-map data 'headline
      (lambda (h)
        (let ((lineage (org-element-lineage h)))
          (when (and (equal (org-element-property :level h)
                            yankpad-snippet-heading-level)
                     (member category-name
                             (mapcar (lambda (x)
                                       (org-element-property :raw-value x))
                                     lineage)))
            h))))))

(defun yankpad--snippets (category-name)
  "Get an alist of the snippets in CATEGORY-NAME.
The car is the snippet name and the cdr is the snippet string."
  (mapcar (lambda (h)
            (let ((heading (org-element-property :raw-value h))
                  (text (org-element-map h 'section #'org-element-interpret-data)))
              (cons heading
                    (or text 
                        (list (concat "\"" heading "\" is an empty category. Please check your yankpad file..."))))))
          (yankpad--snippet-elements category-name)))

(defun yankpad-map-category-keybindings ()
  "Replaces `yankpad-map' with current `yankpad-category' snippet bindings.
Searches all snippets and takes their last tag and interprets it as a key binding."
  (setq yankpad-map nil)
  (define-prefix-command 'yankpad-map)
  (mapc (lambda (h)
          (let* ((last-tag (car (last (org-element-property :tags h))))
                 (text (and last-tag
                            (car (org-element-map h 'section #'org-element-interpret-data)))))
            (when (and text last-tag)
              (define-key yankpad-map (kbd (substring-no-properties last-tag))
                `(lambda ()
                   (interactive)
                   (yankpad--insert-snippet-text ,(substring-no-properties text)))))))
        (yankpad--snippet-elements yankpad-category)))

(add-hook 'yankpad-switched-category-hook #'yankpad-map-category-keybindings)

(defun yankpad-local-category-to-major-mode ()
  "Try to change `yankpad-category' to match the buffer's major mode.
If successful, make `yankpad-category' buffer-local."
  (when (file-exists-p yankpad-file)
    (let ((category (car (member (symbol-name major-mode)
                                 (yankpad--categories)))))
      (when category (yankpad-set-local-category category)))))

(add-hook 'after-change-major-mode-hook #'yankpad-local-category-to-major-mode)

(defun yankpad-local-category-to-projectile ()
  "Try to change `yankpad-category' to match the `projectile-project-name'.
If successful, make `yankpad-category' buffer-local."
  (when (and (require 'projectile nil t)
             (file-exists-p yankpad-file))
    (let ((category (car (member (projectile-project-name)
                                 (yankpad--categories)))))
      (when category (yankpad-set-local-category category)))))

(add-hook 'projectile-find-file-hook #'yankpad-local-category-to-projectile)

(provide 'yankpad)
;;; yankpad.el ends here
