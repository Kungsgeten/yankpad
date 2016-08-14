;;; yankpad.el --- Paste snippets from an org-mode file

;; Copyright (C) 2016 Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand
;; URL: http://github.com/Kungsgeten/yankpad
;; Version: 1.20
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
;; You can also add keybindings to snippets, by setting an `org-mode' tag on the
;; snippet.  The last tag will be interpreted as a keybinding, and the snippet
;; will then be inserted into `yankpad-map'.  `yankpad-map' is not bound to any
;; key by default.
;;
;; Another functionality is that snippets can include function calls, instead of
;; text.  In order to do this, the snippet heading should have a tag named
;; "func".  The snippet name could either be the name of the elisp function that
;; should be executed (will be called without arguments), or the content of the
;; snippet could be an `org-mode' src-block, which will then be executed when
;; you use the snippet.
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
;;    If yanking a snippet into org-mode, this will respect the
;;    current tree level by default.  Set the variable
;;    `yankpad-respect-current-org-level' to nil in order to change that.
;;
;; * Category 2
;; ** Snippet 1
;;
;;    This is yet another snippet, in a different category.
;; ** Snippet 2        :s:
;;
;;    This snippet will be bound to "s" in `yankpad-map'.  Let's say you bind
;;    `yankpad-map' to f7, you can now press "f7 s" to insert this snippet.
;;
;; ** magit-status          :func:
;; ** Run magit-status      :func:m:
;;    #+BEGIN_SRC emacs-lisp
;;    (magit-status)
;;    #+END_SRC
;;
;;; Code:

(require 'org-element)
(when (version< (org-version) "8.3")
  (require 'ox))

(defvar yankpad-file (expand-file-name "yankpad.org" org-directory)
  "The path to your yankpad.")

(defvar yankpad-category nil
  "The current yankpad category.  Change with `yankpad-set-category'.")
(put 'yankpad-category 'safe-local-variable #'stringp)

(defvar yankpad-category-heading-level 1
  "The `org-mode' heading level of categories in the `yankpad-file'.")

(defvar yankpad-snippet-heading-level 2
  "The `org-mode' heading level of snippets in the `yankpad-file'.")

(defvar yankpad-respect-current-org-level t
  "Whether to respect `org-current-level' when using \* in snippets and yanking them into `org-mode' buffers.")

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
  "Insert TEXT into buffer.
Use yasnippet if available."
  (if (and (require 'yasnippet nil t)
           yas-minor-mode)
      (if (region-active-p)
          (yas-expand-snippet text (region-beginning) (region-end))
        (yas-expand-snippet text))
    (insert text)))

(defun yankpad--trigger-snippet-function (snippetname content)
  "SNIPPETNAME can be an elisp function, without arguments, if CONTENT is nil.
If non-nil, CONTENT should hold a single `org-mode' src-block, which will be executed."
  (if (car content)
      (with-temp-buffer
        (delay-mode-hooks
          (org-mode)
          (insert (car content))
          (goto-char (point-min))
          (when (org-in-src-block-p)
            (org-babel-execute-src-block))))
    (if (intern-soft snippetname)
        (funcall (intern-soft snippetname))
      (message (concat "\"" snippetname "\" isn't a function.")))))

(defun yankpad--run-snippet (snippet)
  "Triggers the SNIPPET behaviour."
  (let ((name (car snippet))
        (tags (cadr snippet))
        (content (cddr snippet)))
    (cond
     ((member "func" tags)
      (yankpad--trigger-snippet-function name content))
     (t
      (if (car content)
          ;; Respect the tree levl when yanking org-mode headings.
          (let ((prepend-asterisks 1))
            (when (and yankpad-respect-current-org-level
                       (equal major-mode 'org-mode)
                       (org-current-level))
              (setq prepend-asterisks (org-current-level)))
            (yankpad--insert-snippet-text
             (replace-regexp-in-string
              "^\\\\[*]" (make-string prepend-asterisks ?*) (car content))))
        (message (concat "\"" name "\" snippet doesn't contain any text. Check your yankpad file.")))))))

(defun yankpad-insert-from-category (category)
  "Choose a yankpad entry from CATEGORY.
Does not change `yankpad-category'."
  (let* ((snippets (yankpad--snippets category))
         (snippet (completing-read "Snippet: " snippets)) )
    (yankpad--run-snippet (assoc snippet snippets))))

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
  "Get all the snippet `org-mode' heading elements in CATEGORY-NAME."
  (let ((data (yankpad--file-elements)))
    (org-element-map data 'headline
      (lambda (h)
        (let ((lineage (if (version< (org-version) "8.3")
                           (org-export-get-genealogy h)
                         (org-element-lineage h))))
          (when (and (equal (org-element-property :level h)
                            yankpad-snippet-heading-level)
                     (member category-name
                             (mapcar (lambda (x)
                                       (org-element-property :raw-value x))
                                     lineage)))
            h))))))

(defun yankpad--snippets (category-name)
  "Get an alist of the snippets in CATEGORY-NAME.
The car is the snippet name and the cdr is a cons (tags snippet-string)."
  (mapcar (lambda (h)
            (let ((heading (org-element-property :raw-value h))
                  (text (org-element-map h 'section #'org-element-interpret-data))
                  (tags (org-element-property :tags h)))
              (cons heading (cons tags text))))
          (yankpad--snippet-elements category-name)))

(defun yankpad-map-category-keybindings ()
  "Replace `yankpad-map' with current `yankpad-category' snippet bindings.
Searches all snippets and takes their last tag and interprets it as a key binding."
  (setq yankpad-map nil)
  (define-prefix-command 'yankpad-map)
  (mapc (lambda (h)
          (let ((last-tag (car (last (org-element-property :tags h)))) )
            (when (and last-tag (not (eq last-tag "func")))
              (let ((heading (org-element-property :raw-value h))
                    (content (org-element-map h 'section #'org-element-interpret-data))
                    (tags (org-element-property :tags h)))
                (define-key yankpad-map (kbd (substring-no-properties last-tag))
                  `(lambda ()
                     (interactive)
                     (yankpad--run-snippet (cons ,heading
                                                 (cons (list ,@tags)
                                                       (list ,@content))))))))))
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

(eval-after-load "projectile"
  (add-hook 'projectile-find-file-hook #'yankpad-local-category-to-projectile))

(provide 'yankpad)
;;; yankpad.el ends here
