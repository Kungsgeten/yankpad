#+TITLE:yankpad.el [[https://melpa.org/#/yankpad][file:https://melpa.org/packages/yankpad-badge.svg]]

- [[https://kungsgeten.github.io/yankpad.html][Blog post about Yankpad]]
- [[https://youtu.be/xkkyE7d0Bpc][Screencast of Yankpad]] (thanks [[https://github.com/zeltak][zeltak]])

Let's say that you have text snippets that you want to paste, but that [[https://joaotavora.github.io/yasnippet/][yasnippet]]
or [[https://www.emacswiki.org/emacs/SkeletonMode][skeleton]] is a bit too much when you do not need a shortcut/abbrev for your
snippet. You like org-mode, so why not write your snippets there? Introducing
the yankpad:

#+BEGIN_SRC org
  ,* Category 1

  ,** Snippet title

      Here's a text snippet I want to insert.

  ,** Snippet with keybinding                               :last:tag:is:key:o:

      And here's another snippet. This snippet has tags, and the last of these
      tags should be a key. This will bind the snippet to the key (in this case
      "o") when first calling yankpad-map.

  ,** expandword: Snippet with keyword expansion

      This snippet has a keyword; "expandword" in this case. If this category is
      active, and you type the keyword into a buffer and use the "yankpad-expand"
      command, the keyword will be replaced with this snippet.

  ,** more:expands: Multiple keywords

      A snippet can have more than one keyword. This has both "more" and
      "expands".

  ,** Regex expands                      :props:
     :PROPERTIES:
     :YP_EXPAND_REGEX: number\([[:digit:]]+\)
     :END:

     If you use the :props: tag the property drawer will not be included in the
     snippet. Instead the snippet can include information used by Yankpad.

     In this case we have set the property YP_EXPAND_REGEX which can be used instead
     of the expand keyword. YP_EXPAND_REGEX should be a regular expression, and when
     you use "yankpad-expand" the regex will be replaced with the snippet.

     The cool thing here is that the entire snippet text is then, before
     expansion, sent into the Emacs "format" function, with the OBJECTS argument
     set to the match groups in the regex. %s

     In this example, the "percent s" at the end of the last paragraph will be
     replaced with the digit matched by the regex. So if I write "number12" and
     use "yankpad-expand" the "percent s" will be replaced with 12.

  ,* Category 2

    Descriptive lists will be treated as snippets. You can set them to be treated
    as =abbrev-mode= abbrevs instead, by setting
    =yankpad-descriptive-list-treatment= to abbrev. If a heading could be
    considered to be a snippet, add the =snippetlist= tag to ignore the snippet
    and scan it for descriptive lists instead.

    - name :: Erik Sjöstrand
    - key :: Typing "key" followed by `yankpad-expand' will insert this snippet.

  ,** Descriptive list example 2                  :snippetlist:

     This heading would normally be considered a snippet, but because of the
     =:snippetlist:= tag, it is scanned for descriptive lists instead.

     - foo :: bar

  ,** Explaining categories

      This snippet belongs to another category (named =Category 2=). Categories
      are useful if you need several yankpads, for instance if you're a teacher
      (like me) working with different courses.

  ,** yasnippet magic

      If you have yasnippet installed (not a requirement), the content in each
      snippet is actually executed by yasnippet! This means that you could run
      elisp inside your snippets: `(+ 3 4)` and have handy tab stop fields.

      | Student | Grade |
      |---------+-------|
      | $1      | $2    |

      That's pretty handy!
      $0

  ,** [[file:my_other_snippets.org]]

     If a heading has a link to another org-file, that file will be scanned for
     snippets. Those snippets are then appended to the category.

  ,** [[file:misc_snippets::*Search]]

     You can specify a specific headline in another file, which you want to be
     searched for snippets. It could be a single snippet, or it could have
     subtrees (in which case all of them will be considered as snippets).

  ,** [[id:38e4c8d2-5ab0-4e78-8e43-ea4a918e5c02]]

     You can also provide the ID of a specific org-mode headline.

  ,** Code snippet examples

      You can organize your snippets inside a category by using subtrees, like
      this one. Only headings without children are considered as snippets.

  ,*** "Litterate programming" snippet                    :src:

       Tagging a snippet with src says that only the content of source blocks
       should be expanded. All other text (like this paragraph) is ignored.

       ,#+BEGIN_SRC emacs-lisp
       (message "This is part of the snippet")
       ,#+END_SRC

       If you have several source blocks, their content will be concatenated.

       ,#+BEGIN_SRC emacs-lisp
       (message "This is also part of the snippet!!!")
       ,#+END_SRC

  ,*** The source block below will be executed if tag is func :func:
       ,#+BEGIN_SRC emacs-lisp
       ;; Instead of a src-block, the snippet may be named
       ;; the same as an emacs-lisp function. This will then
       ;; be executed without arguments (see next example).
       (elfeed)
       ,#+END_SRC

  ,** elfeed                                            :func:e:

  ,* Kitchen sink category
  :PROPERTIES:
  :INCLUDE:  Category 1|Category 2
  :END:

  ,** Include other categories

  Snippets from Category 1 and Category 2 will be appended to this category.
  This is done by setting the INCLUDE property of the category. Categories
  are separated by a pipe.

  ,* org-mode

  ,** Major-mode categories

      If you have a category with the same name as a major-mode, that category will be
      activated when switching major-mode. This only affects the local buffer and does
      not modify the global category.

  ,* my-projectile-project

  ,** Projectile based categories

      If you have projectile installed (not a requirement) you can give a category
      the same name as one of your projectile projects. That category will be
      activated when using projectile-find-file on a file in the project.

  ,* Global category                                   :global:
  ,** Always available

      Snippets in a category with the :global: tag are always available for
      expansion.

  ,* Default                                           :global:
  ,** Fallback for major-mode categories

     If you open a file, but have no category named after its major-mode, a
     category named "Default" will be used instead (if you have it defined in your
     Yankpad). It is probably a good idea to make this category global. You can
     change the name of the default category by setting the variable
     yankpad-default-category.
#+END_SRC

* Setup

1. Install =yankpad= from Melpa, or download =yankpad.el= and add it to your load-path and require it.
2. The default location for the yankpad file is =yankpad.org= in your =org-directory=. This can be changed by modifying the =yankpad-file= variable.
3. Optionally bind =yankpad-map=, =yankpad-insert=, and/or =yankpad-expand= to a key.
4. Optionally install =yasnippet= and/or =projectile= and/or =company-mode=, if you want the additional yankpad features that those package provide.
5. That's it!

If you want different heading levels for the categories (default 1), change the value of =yankpad-category-heading-level=. You can also change the tag which defines categories as global, by modifying =yankpad-global-tag=. The name of the major-mode fallback category can be changed by modifying =yankpad-default-category=.

At the beginning of your snippet title you may have a list of keywords. These keywords are separated by colons (=:=). For the most part you probably only need one keyword, like =hello: Greetings!=, but you may have several keywords for the same snippet: =hello:hi: Greetings!=. You can change =:= into another string by changing the =yankpad-expand-separator= variable.

Here's an example setup using the excellent [[https://github.com/jwiegley/use-package][use-package]]:

#+BEGIN_SRC emacs-lisp
  (use-package yankpad
    :ensure t
    :defer 10
    :init
    (setq yankpad-file "~/yankpad.org")
    :config
    (bind-key "<f7>" 'yankpad-map)
    (bind-key "<f12>" 'yankpad-expand)
    ;; If you want to complete snippets using company-mode
    (add-to-list 'company-backends #'company-yankpad)
    ;; If you want to expand snippets with hippie-expand
    (add-to-list 'hippie-expand-try-functions-list #'yankpad-expand))
#+END_SRC

* Usage

1. Add snippet entries to your =yankpad-file=. Level 1 headings are considered to be categories (by default). Also descriptive lists are treated as snippets by default (except if they're in a heading without children, in which case the heading needs a =:snippetlist:= if it should be scanned for descriptive lists). A quick way to open your =yankpad-file= is to use =M-x yankpad-edit=. You can also add snippets to the current =yankpad-category= by using =M-x yankpad-capture=, or with =M-x yankpad-aya-persist= if you're a [[https://github.com/abo-abo/auto-yasnippet][auto-yasnippet]] user.
2. Insert a snippet with =M-x yankpad-insert=. If the snippet has a keyword (it starts with a word followed by a colon), you can write that keyword into the buffer and use =M-x yankpad-expand= instead. It may be useful to bind these commands to some key on your keyboard. You can also use =company-yankpad= to expand a snippet using =company-mode= (thanks [[https://github.com/sid-kurias][sid-kurias]] for contributing). If you want to insert the last snippet again, you can use =M-x yankpad-repeat= (bind that to a key if you're using it frequently).
3. If you want to change category, use =M-x yankpad-set-category=. If you have a category with the same name as a major-mode (for instance =org-mode=), that category will be locally set when switching major-mode. In the same manner you can name a category to one of your Projectile project names (if Projectile is installed). If both cases are true, the Projectile category becomes active, but the snippets from the major mode are appended as well. If you later change category with =M-x yankpad-set-category=, the major-mode and project snippets will be appended to the chosen category.
4. If you want to append snippets from another of your categories (basically like having two or more categories active at the same time), use =M-x yankpad-append-category=. If you want one of your categories to /always/ include snippets from another category; set the =INCLUDE= [[https://orgmode.org/manual/Property-syntax.html#Property-syntax][property]] of the category heading (several categories can be included this way, by separating them with =|=, see example at the top of this readme).
5. To quickly open your =yankpad-file= for editing, run =M-x yankpad-edit=.
6. Yankpad caches your snippets, making it a bit snappier to insert snippets from the yankpad. If you've edited your =yankpad-file= you might want to use =M-x yankpad-reload= to clear the snippet cache and reload your snippets in the current category.

Since a =*= at the beginning of a line would specify a new heading, lines can not begin with =*=. However, you can write =\*= at the beginning of a line, which will be replaced by a =*= when expanding the snippet. If you use this in order to yank snippets into an =org-mode= buffer, the new headings will be automatically indented -- depending on the current level -- by default. This can be changed by setting the variable =yankpad-respect-current-org-level= to =nil=, or by using special tags. Another approach is to encapsulate the snippet text in an /org src block/ and tag the snippet with =src= (see /Special tags/ below).

Sometimes it may be useful to set the category automatically for a specific file. In this case you can add =yankpad-category= as a [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Specifying-File-Variables.html][file variable]], for instance by adding this line at the top of your file:

#+BEGIN_SRC
-*- yankpad-category: "Category name"; -*-
#+END_SRC

You can also set the =yankpad-category= to =nil= in this way, if you do not want any default category triggered for that file.

There's a macro called =yankpad-map-simulate= which can be used if you want a command which presses a specific key inside =yankpad-map=, for instance if you want a special keybinding for a specific snippet. The macro will create a command named =yankpad-map-press-<key>=. Here's an example of how you could create a command and bind it to a key:

#+BEGIN_SRC emacs-lisp
  (global-set-key (kbd "<f5>") (yankpad-map-simulate "j"))
#+END_SRC

Now pressing =f5= would trigger the snippet bound to =j= inside =yankpad-map=.

* Special tags

Snippets in your Yankpad can have tags, and some of these have special meanings:

- =src= :: If a snippet has a tag named =src=, all code in [[https://orgmode.org/guide/Working-With-Source-Code.html][source blocks]] inside the snippet will be concatenated -- becoming the new snippet. All other text inside the snippet is discarded, so it can be used as comments for the source blocks. This feature is inspired by [[https://github.com/tuhdo/org-recipes][org-recipes]].
- props :: The =props= tag removes the property drawer from the snippet, before expanding. The properties in the drawer can be used to collect data from the snippet. See "Special properties" below.
- =func= :: If a snippet has a tag named =func=, it won't insert text. Instead a function will be executed upon "inserting" the snippet. The name of the snippet can be an elisp function, which will be run without arguments. Instead, the function could hold a single =org-mode= src-block, which will be executed in a separate buffer (so the code in the src-block does not have access to the current buffer).
- =results= :: Works like =func=, but the output of the function will be inserted into the buffer.
- =indent_nil= :: By default the inserted text will be indented (uses =indent_region= or the settings of =yas-indent-line= if =yas-minor-mode= is active). By using =indent_nil=, no indentation will occur.
- =indent_auto= :: Sets =yas-indent-line= to =auto= for this snippet.
- =indent_fixed= :: Sets =yas-indent-line= to =fixed= for this snippet.
- =wrap= :: Sets =yas-wrap-around-region= to =t= for this snippet.
- =wrap_nil= :: Sets =yas-wrap-around-region= to =nil= for this snippet.
- =<key>= :: The last tag of a snippet (except if its one of the above) will add the tag as a keybinding when first calling =yankpad-map=. If the last tag is =o=, then using =M-x yankpad-map o= will insert that snippet. This is most useful if you bind =yankpad-map= to a key. You can also have multiple letters in the tag, which will be treated as key sequences: if the last tag is =yy= then =M-x yankpad-map y y= will trigger it.
- =snippetlist= :: With this tag, =yankpad= will not actually consider the heading to be a snippet. Instead it will scan the heading for descriptive lists and treat them according to =yankpad-descriptive-list-treatment=.
- orglevel :: As if =yankpad-respect-current-org-level= were true for this snippet.
- no_orglevel :: As if =yankpad-respect-current-org-level= were false for this snippet.
* Special properties

If a snippet has a property drawer, and the =:props:= tag, the drawer will be removed from the snippet text and the properties will be stored in the snippet. At the moment there's only one property that has an effect on Yankpad's behaviour, but more might be added in the future.

- =YP_EXPAND_REGEX= :: If this property is set, it will be used /instead/ of any expansion keywords in the snippet title. The property is a regular expression. When using =yankpad-expand= the snippet will be expanded if the symbol at point matches the regex. Before expansion the snippet content will be sent to Emacs' =format= function, where the =OBJECTS= argument is the matched groups from the regex. Example: If you have =source_\(.+\)= as the regex property, and expand =source_python=, the first =%s= in your snippet text will be replaced with =python=.

You could add your own special properties using =yankpad-before-snippet-hook=. This hook is run before a snippet is inserted, and the hook functions should take the snippet as their only argument. A snippet is a list with four elements: =(snippet name, a list of tags, content, an alist of properties)=. If you use =setf= on the snippet, you can change it before expansion. Here's an example that would upcase a snippet if it includes the =UPCASE= property:

#+BEGIN_SRC emacs-lisp
(defun yp/upcase-snippet (snippet)
  ;; Check if we have a property named UPCASE
  ;; (nth 4 snippet) holds all the properties
  (when (assoc "UPCASE" (nth 4 snippet))
    ;; (nth 3 snippet) is the snippet content, let's upcase it!
    (setf (nth 3 snippet)
          (upcase (nth 3 snippet)))))

(add-hook 'yankpad-before-snippet-hook 'yp/upcase-snippet)
#+END_SRC

* Integration with =abbrev-mode=

If you set =yankpad-descriptive-list-treatment= to ='abbrev=, descriptive lists inside =yankpad= categories will be handled by =abbrev-mode= instead of being considered as snippets.

* Changelog

- 2.30 (September 2019) :: Added property functionality via the tag =props=. Added regex expansion with the =YP_EXPAND_REGEX= property. Added =yankpad-before-snippet-hook=.
- 2.20 (November 2018) :: Added the variable =yankpad-default-category=, which is =Default= by default. If you have a category with this name, it will be used for the current file if you don't have a major-mode specific category for that file. A snippet can now have several expand keywords, just separate them with colons.
- 2.15 (June 2018) :: Descriptive lists defining snippets can now be placed anywhere under a category, and not only at the =yankpad-category-level=. If placed in a heading without children, the heading needs the =:snippetlist:= tag (otherwise it will be considered to be a normal snippet).
- 2.10 (April 2018) :: Snippets can be spread between files, by using links in snippet headlines. Only headlines without subtrees are considered to be snippets, which means you can organize your snippets in different subtrees. =yankpad-snippet-heading-level= is removed, since it isn't needed anymore.
- 2.00 (March 2018) :: Snippets, with keywords, may now be defined in descriptive lists. These lists could instead be treated by =abbrev-mode=. A category can be tagged as =:global:= in order to include its snippets in all categories.
- 1.90 (March 2018) :: Added =yankpad-map-simulate=. =yankpad-map= has a helper text (thanks [[https://github.com/akirak][akirak]]). =wrap= tags has been added. =yankpad-aya-persist= for [[https://github.com/abo-abo/auto-yasnippet][auto-yasnippet]] added.
- 1.80 (February 2018) :: Snippets can be configured to concatenate the [[https://orgmode.org/guide/Working-With-Source-Code.html][source blocks]] in the snippet. This is done by adding the =src= tag to the snippet.
- 1.70 (February 2017) :: =yankpad-repeat= and =yankpad-capture-snippet= added.
- 1.60 (January 2017) :: =company-yankpad= (requires [[https://company-mode.github.io/][company-mode]]) was contributed by [[https://github.com/sid-kurias][sid-kurias]]. You can now use company to complete snippet names!
- 1.51 (January 2017) :: Added =yankpad-reload=.
- 1.50 (September 2016) :: It is now possible to have active snippets from several categories at once, by using =M-x yankpad-append-category= or by modifying the yankpad file. This is done automatically for major mode and projectile categories.
- 1.40 (August 2016) :: Added =results= tag. Works as =func= tag, but the output of the function is inserted into the buffer.
- 1.31 (August 2016) :: Snippets are indented as default. The indentation behaviour can be changed by using =indent_nil=, =indent_fixed=, or =indent_auto= as tags for the snippet(s).
- 1.30 (August 2016) :: Snippets can now have keywords. If typing the snippet keyword into the buffer, the snippet can be expanded by calling =yankpad-expand=. Just name the snippet =expandword: Snippet name= and you can type =expandword M-x yankpad-expand= to insert it.
- 1.20 (July 2016) :: Snippets can be used to execute functions, instead of inserting text. Add the tag =func= to your snippet. The snippet can contain an =org-mode= src-block, which will be executed, or the snippet may be named the same as an emacs-lisp function, which will be executed without arguments.
- 1.10 (May 2016) :: Snippets can have keybindings by tagging them. The last tag will be interpreted as a key and inserted into =yankpad-map=.
