;; yankpad.el --- Paste snippets from an org-mode file
;; Copyright (C) 2016 Erik Sjöstrand
;; MIT License
;;
;; A way to insert text snippets from an org-mode file. The org-mode file in
;; question is defined in `yankpad-file' and is set to "yankpad.org" in your
;; `org-directory' by default. In this file, each heading specifies a snippet
;; category and each subheading of that category specifies a snippet. This way
;; you can have different yankpads for different occasions.
;;
;; To insert a snippet from the yankpad, use `yankpad-insert'. If you need to
;; change the category, use `yankpad-set-category'. Here's an example of what yankpad.org could look like:
;;  
;; * Category 1
;; ** Snippet 1
;; 
;;    This is a snippet.
;; 
;; ** Snippet 2
;;
;;    This is another snippet
;; * Category 2
;; ** Snippet 1
;;
;;    This is yet another snippet, in a different category.

(defvar yankpad-file (expand-file-name "yankpad.org" org-directory))
(defvar yankpad-category nil)

(defun yankpad-set-category ()
  "Change the yankpad category."
  (interactive)
  (setq yankpad-category
        (completing-read "Category: "
                         (yankpad--categories))))

;;;###autoload
(defun yankpad-insert ()
  "Insert an entry from the yankpad."
  (interactive)
  (unless yankpad-category (yankpad-set-category))
  (let* ((snippets (yankpad--snippets yankpad-category))
         (snippet (completing-read "Snippet: " snippets))
         (text (cadr (assoc snippet snippets))))
    (if (require 'yasnippet nil t)
        (if (region-active-p)
            (yas-expand-snippet text (region-beginning) (region-end))
          (yas-expand-snippet text))
      (insert text))))

(defun yankpad-edit ()
  "Opens the yankpad file for editing."
  (interactive)
  (find-file yankpad-file))

(defun yankpad--file-elements ()
  (with-temp-buffer
    (insert-file-contents yankpad-file) 
    (org-element-parse-buffer)))

(defun yankpad--categories ()
  (let ((data (yankpad--file-elements)))
    (org-element-map data 'headline
      (lambda (h)
        (when (equal (org-element-property :level h) 1)
          (org-element-property :raw-value h))))))

(defun yankpad--snippets (category-name)
  (let ((data (yankpad--file-elements)))
    (org-element-map data 'headline
      (lambda (h)
        (let ((parent (org-element-property :parent h)))
          (when (and (equal (org-element-property :level h) 2)
                     (equal (org-element-property :raw-value parent) category-name))
            (cons
             (org-element-property :raw-value h)
             (org-element-map h 'section
               #'org-element-interpret-data))))))))

(provide 'yankpad)
