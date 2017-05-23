;;; yankpad.el --- Paste snippets from an org-mode file

;; Copyright (C) 2016 Erik Sjöstrand
;; MIT License, except company-yankpad and company-yankpad--name-or-key (GPL 3)

;; Author: Erik Sjöstrand
;; URL: http://github.com/Kungsgeten/yankpad
;; Version: 1.70
;; Keywords: abbrev convenience
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; A way to insert text snippets from an org-mode file.  The org-mode file in
;; question is defined in `yankpad-file' and is set to "yankpad.org" in your
;; `org-directory' by default.  In this file, each heading specifies a snippet
;; category and each subheading of that category defines a snippet.  This way
;; you can have different yankpads for different occasions.
;;
;; You can open your `yankpad-file' by using `yankpad-edit' (or just open it in
;; any other way).  Another way to add new snippets is by using
;; `yankpad-capture-snippet', which will add a snippet to the current
;; `yankpad-category'.
;;
;; If you have yasnippet installed, yankpad will try to use it when pasting
;; snippets.  This means that you can use the features that yasnippet provides
;; (tab stops, elisp, etc).  You can use yankpad without yasnippet, and then the
;; snippet will simply be inserted as is.
;;
;; You can also add keybindings to snippets, by setting an `org-mode' tag on the
;; snippet.  The last tag will be interpreted as a keybinding, and the snippet
;; can be run by using `yankpad-map' followed by the key.  `yankpad-map' is not
;; bound to any key by default.
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
;; be switched to when using `projectile-find-file'.  These snippets will be
;; appended to your active snippets if you change category.
;;
;; To insert a snippet from the yankpad, use `yankpad-insert' or
;; `yankpad-expand'.  `yankpad-expand' will look for a keyword at point, and
;; expand a snippet with a name starting with that word, followed by
;; `yankpad-expand-separator' (a colon by default).  If you need to change the
;; category, use `yankpad-set-category'.  If you want to append snippets from
;; another category (basically having several categories active at the same
;; time), use `yankpad-append-category'.  If you have company-mode enabled,
;; you can also use `company-yankpad`.
;;
;; For further customization, please see the Github page: https://github.com/Kungsgeten/yankpad
;;
;; Here's an example of what yankpad.org could look like:

;;; Yankpad example:

;; * Category 1
;; ** Snippet 1
;;
;;    This is a snippet.
;;
;; ** snip2: Snippet 2
;;
;;    This is another snippet.  This snippet can be expanded by first typing "snip2" and
;;    then executing the `yankpad-expand' command.
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
;;    This snippet will be bound to "s" when using `yankpad-map'.  Let's say you
;;    bind `yankpad-map' to f7, you can now press "f7 s" to insert this snippet.
;;
;; ** magit-status          :func:
;; ** Run magit-status      :func:m:
;;    #+BEGIN_SRC emacs-lisp
;;    (magit-status)
;;    #+END_SRC
;;
;; * org-mode
;; ** Snippet 1
;;    This category will be switched to automatically when visiting an org-mode buffer.
;;
;; * Category 3
;;   :PROPERTIES:
;;   :INCLUDE:  Category 1|Category 2
;;   :END:
;; ** A snippet among many!
;;    This category will include snippets from Category 1 and Category 2.
;;    This is done by setting the INCLUDE property of the category.
;;
;;; Code:

(require 'org-element)
(require 'org-capture)
(require 'thingatpt)
(when (version< (org-version) "8.3")
  (require 'ox))

(defvar yankpad-file (expand-file-name "yankpad.org" org-directory)
  "The path to your yankpad.")

(defvar yankpad-category nil
  "The current yankpad category.  Change with `yankpad-set-category'.")
(put 'yankpad-category 'safe-local-variable #'string-or-null-p)

(defvar yankpad-category-heading-level 1
  "The `org-mode' heading level of categories in the `yankpad-file'.")

(defvar yankpad-snippet-heading-level 2
  "The `org-mode' heading level of snippets in the `yankpad-file'.")

(defvar yankpad-respect-current-org-level t
  "Whether to respect `org-current-level' when using \* in snippets and yanking them into `org-mode' buffers.")

(defvar yankpad-switched-category-hook nil
  "Hooks run after changing `yankpad-category'.")

(defvar yankpad-expand-separator ":"
  "String used to separate a keyword, at the start of a snippet name, from the title.  Used for `yankpad-expand'.")

(defvar yankpad--active-snippets nil
  "A cached version of the snippets in the current category.")

(defvar yankpad--last-snippet nil
  "The last snippet executed. Used by `yankpad-repeat'.")

(defun yankpad-active-snippets ()
  "Get the snippets in the current category."
  (or yankpad--active-snippets (yankpad-set-active-snippets)))

;;;###autoload
(defun yankpad-set-category ()
  "Change the yankpad category."
  (interactive)
  (setq yankpad-category
        (completing-read "Category: " (yankpad--categories)))
  (run-hooks 'yankpad-switched-category-hook))

(defun yankpad-set-local-category (category)
  "Set `yankpad-category' to CATEGORY locally."
  (set (make-local-variable 'yankpad-category) category)
  (set (make-local-variable 'yankpad--active-snippets) nil)
  (run-hooks 'yankpad-switched-category-hook))

(defun yankpad-set-active-snippets ()
  "Set the `yankpad--active-snippets' to the snippets in the active category.
If no active category, call `yankpad-set-category'.
Also append major mode and/or projectile categories if `yankpad-category' is local."
  (if yankpad-category
      (progn
        (setq yankpad--active-snippets (yankpad--snippets yankpad-category))
        (when (local-variable-p 'yankpad-category)
          (let ((major-mode-category (car (member (symbol-name major-mode)
                                                  (yankpad--categories)))))
            (when major-mode-category
              (yankpad-append-category major-mode-category)))
          (when (require 'projectile nil t)
            (let ((projectile-category (car (member (projectile-project-name)
                                                    (yankpad--categories)))))
              (when projectile-category
                (yankpad-append-category projectile-category)))))
        yankpad--active-snippets)
    (yankpad-set-category)
    (yankpad-set-active-snippets)))

(defun yankpad-append-category (&optional category)
  "Add snippets from CATEGORY into the list of active snippets.
Prompts for CATEGORY if it isn't provided."
  (interactive)
  (unless category
    (setq category (completing-read "Category: " (yankpad--categories))))
  (unless (equal category yankpad-category)
    (unless yankpad--active-snippets (yankpad-set-active-snippets))
    (setq yankpad--active-snippets
          (append yankpad--active-snippets (yankpad--snippets category)))))

(defun yankpad-reload ()
  "Clear the snippet cache.
The next try to `yankpad-insert` will reload `yankpad-file`.
Useful to run after editing the `yankpad-file`."
  (interactive)
  (setq yankpad--active-snippets nil))

(add-hook 'yankpad-switched-category-hook #'yankpad-reload)

;;;###autoload
(defun yankpad-insert ()
  "Insert an entry from the yankpad.
Uses `yankpad-category', and prompts for it if it isn't set."
  (interactive)
  (unless yankpad-category
    (or (yankpad-local-category-to-major-mode)
        (yankpad-set-category)))
  (yankpad-insert-from-current-category))

(defun yankpad--insert-snippet-text (text indent)
  "Insert TEXT into buffer.  INDENT is whether/how to indent the snippet.
Use yasnippet and `yas-indent-line' if available."
  (setq text (substring-no-properties text 0 -1))
  (if (and (require 'yasnippet nil t)
           yas-minor-mode)
      (if (region-active-p)
          (yas-expand-snippet text (region-beginning) (region-end) `((yas-indent-line (quote ,indent))))
        (yas-expand-snippet text nil nil `((yas-indent-line (quote ,indent)))))
    (let ((start (point)))
      (insert text)
      (when indent
        (indent-region start (point))))))

(defun yankpad--trigger-snippet-function (snippetname content)
  "SNIPPETNAME can be an elisp function, without arguments, if CONTENT is nil.
If non-nil, CONTENT should hold a single `org-mode' src-block, to be executed.
Return the result of the function output as a string."
  (if (car content)
      (with-temp-buffer
        (delay-mode-hooks
          (org-mode)
          (insert (car content))
          (goto-char (point-min))
          (if (org-in-src-block-p)
              (prin1-to-string (org-babel-execute-src-block))
            (error "No org-mode src-block at start of snippet"))))
    (if (intern-soft snippetname)
        (prin1-to-string (funcall (intern-soft snippetname)))
      (error (concat "\"" snippetname "\" isn't a function")))))

(defun yankpad--run-snippet (snippet)
  "Triggers the SNIPPET behaviour."
  (setq yankpad--last-snippet snippet)
  (let ((name (car snippet))
        (tags (cadr snippet))
        (content (cddr snippet)))
    (cond
     ((member "func" tags)
      (yankpad--trigger-snippet-function name content))
     ((member "results" tags)
      (insert (yankpad--trigger-snippet-function name content)))
     (t
      (if (car content)
          ;; Respect the tree level when yanking org-mode headings.
          (let ((prepend-asterisks 1)
                (indent (cond ((member "indent_nil" tags)
                               nil)
                              ((member "indent_fixed" tags)
                               'fixed)
                              ((member "indent_auto" tags)
                               'auto)
                              ((and (require 'yasnippet nil t) yas-minor-mode)
                               yas-indent-line)
                              (t t))))
            (when (and yankpad-respect-current-org-level
                       (equal major-mode 'org-mode)
                       (org-current-level))
              (setq prepend-asterisks (org-current-level)))
            (yankpad--insert-snippet-text
             (replace-regexp-in-string
              "^\\\\[*]" (make-string prepend-asterisks ?*) (car content))
             indent))
        (message (concat "\"" name "\" snippet doesn't contain any text. Check your yankpad file.")))))))

(defun yankpad-repeat ()
  "Repeats the last used snippet."
  (interactive)
  (if yankpad--last-snippet
      (yankpad--run-snippet yankpad--last-snippet)
    (error "There has been no previous snippet")))

(defun yankpad--remove-id-from-yankpad-capture ()
  "Remove ID property from last `yankpad-capture-snippet', save `yankpad-file'."
  (let* ((properties (org-entry-properties org-capture-last-stored-marker))
         (file (cdr (assoc "FILE" properties))))
    (when (equal file yankpad-file)
      (when (org-entry-delete org-capture-last-stored-marker "ID")
        (with-current-buffer (get-file-buffer file)
          (save-buffer))))))
(add-hook 'org-capture-after-finalize-hook #'yankpad--remove-id-from-yankpad-capture)

;;;###autoload
(defun yankpad-capture-snippet ()
  "`org-capture' a snippet to current `yankpad-category' (prompts if not set)."
  (interactive)
  (unless yankpad-category
    (yankpad-set-category))
  (let ((org-capture-entry
         `("y" "Yankpad" entry (file+headline ,yankpad-file ,yankpad-category)
           "* %?\n%i")))
    (org-capture))
  (yankpad-reload))

(defun yankpad-insert-from-current-category (&optional name)
  "Insert snippet NAME from `yankpad-category'.  Prompts for NAME unless set.
Does not change `yankpad-category'."
  (let ((snippets (yankpad-active-snippets)))
    (unless name
      (setq name (completing-read "Snippet: " snippets)))
    (let ((snippet (assoc name (yankpad-active-snippets))))
      (if snippet
          (yankpad--run-snippet snippet)
        (message (concat "No snippet named " name))
        nil))))

;;;###autoload
(defun yankpad-expand ()
  "Replace word at point with a snippet.
Only works if the word is found in the first matching group of `yankpad-expand-keyword-regex'."
  (interactive)
  (let* ((word (word-at-point))
         (bounds (bounds-of-thing-at-point 'word))
         (snippet-prefix (concat word yankpad-expand-separator)))
    (when (and word yankpad-category)
      (catch 'loop
        (mapc
         (lambda (snippet)
           (when (string-prefix-p snippet-prefix (car snippet))
             (delete-region (car bounds) (cdr bounds))
             (yankpad--run-snippet snippet)
             (throw 'loop snippet)))
         (yankpad-active-snippets))
        nil))))

;;;###autoload
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
  (let ((data (yankpad--file-elements))
        (lineage-func (if (version< (org-version) "8.3")
                          #'org-export-get-genealogy
                        #'org-element-lineage)))
    (org-element-map data 'headline
      (lambda (h)
        (let ((lineage (funcall lineage-func h)))
          (when (and (equal (org-element-property :level h)
                            yankpad-snippet-heading-level)
                     (member category-name
                             (mapcar (lambda (x)
                                       (org-element-property :raw-value x))
                                     lineage)))
            h))))))

(defun yankpad--category-include-property (category-name)
  "Get the \"INCLUDE\" property from CATEGORY-NAME."
  (let ((data (yankpad--file-elements)))
    (org-element-map data 'headline
      (lambda (h)
        (when (and (equal (org-element-property :level h)
                          yankpad-category-heading-level)
                   (equal (org-element-property :raw-value h)
                          category-name))
          (org-element-property :INCLUDE h)))
      nil t)))

(defun yankpad--snippets (category-name)
  "Get an alist of the snippets in CATEGORY-NAME.
The car is the snippet name and the cdr is a cons (tags snippet-string)."
  (let* ((propertystring (yankpad--category-include-property category-name))
         (include (when propertystring
                    (split-string propertystring "|")))
         (snippets
          (mapcar (lambda (h)
                    (let ((heading (org-element-property :raw-value h))
                          (text (org-element-map h 'section #'org-element-interpret-data))
                          (tags (org-element-property :tags h)))
                      (cons heading (cons tags text))))
                  (yankpad--snippet-elements category-name))))
    (append snippets (cl-reduce #'append (mapcar #'yankpad--snippets include)))))

;;;###autoload
(defun yankpad-map ()
  "Create and execute a keymap out of the last tags of snippets in `yankpad-category'."
  (interactive)
  (define-prefix-command 'yankpad-keymap)
  (mapc (lambda (snippet)
          (let ((last-tag (car (last (cadr snippet)))))
            (when (and last-tag
                       (not (eq last-tag "func"))
                       (not (eq last-tag "results"))
                       (not (string-prefix-p "indent_" last-tag)))
              (let ((heading (car snippet))
                    (content (cddr snippet))
                    (tags (cadr  snippet)))
                (define-key yankpad-keymap (kbd (substring-no-properties last-tag))
                  `(lambda ()
                     (interactive)
                     (yankpad--run-snippet (cons ,heading
                                                 (cons (list ,@tags)
                                                       (list ,@content))))))))))
        (yankpad-active-snippets))
  (set-transient-map 'yankpad-keymap))

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

;; `company-yankpad--name-or-key' and `company-yankpad' are Copyright (C) 2017
;; Sidart Kurias (https://github.com/sid-kurias/) and are included by permission
;; from the author. They're licensed under GNU GPL 3 (http://www.gnu.org/licenses/).

(defun company-yankpad--name-or-key (arg fn)
  "Return candidates that match the string entered.
ARG is what the user has entered and expects a match for.
FN is the function that will extract either name or key."
  (delq nil
        (mapcar
         (lambda (c) (let ((snip (split-string (car c)  yankpad-expand-separator)))
                       (if (string-prefix-p arg (car snip) t)
                           (progn
                             (if (string-match yankpad-expand-separator (car c))
                                 (set-text-properties 0 1 '(type keyword) (car snip))
                               (set-text-properties 0 1 '(type name) (car snip)))
                             (funcall fn snip)))))
         (yankpad-active-snippets))))

;;;###autoload
(defun company-yankpad (command &optional arg &rest ignored)
  "Company backend for yankpad."
  (interactive (list 'interactive))
  (if (require 'company nil t)
      (case command
        (interactive (company-begin-backend 'company-yankpad))
        (prefix (company-grab-symbol))
        (annotation (car (company-yankpad--name-or-key
                          arg
                          (lambda (snippet) (mapconcat 'identity (cdr snippet) " ")))))
        (candidates (company-yankpad--name-or-key arg (lambda (snippet) (car snippet))))
        (post-completion (let ((type (get-text-property 0 'type arg)))
                           (if (equal type 'keyword)
                               (yankpad-expand)
                             (let ((word (word-at-point))
                                   (bounds (bounds-of-thing-at-point 'word)))
                               (delete-region (car bounds) (cdr bounds))
                               (yankpad-insert-from-current-category arg)))))
        (duplicates t))
    (error "You need company in order to use company-yankpad")))

(provide 'yankpad)
;;; yankpad.el ends here
