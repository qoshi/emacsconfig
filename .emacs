;;set loadpath
(add-to-list 'load-path' "~/.emacs.d/cl-lib/")
(require 'cl-lib)

(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(add-to-list 'load-path "~/.emacs.d/plugins/theme")
(add-to-list 'load-path "~/.emacs.d/cl-lib")
(add-to-list 'load-path "~/.emacs.d/plugins/gofly")

;;set default coding system utf-8
(setq default-buffer-file-coding-system 'utf-8-unix)
;;close welcome
(setq inhibit-startup-message t)

;;theme
(require 'color-theme)
(color-theme-initialize)
(custom-set-variables
 '(custom-enabled-themes (quote (dichromacy)))
 '(inhibit-startup-screen t)
 )
(set-background-color "#faf9de")

;;templete
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/plugins/yasnippet/snippets"
        ))
(yas-global-mode 1)

;;file list
(require 'sr-speedbar)

;;tabbar
(require 'tabbar)
(tabbar-mode 1)

;;auto complete
(require 'auto-complete-config)
(ac-config-default)
(auto-complete-mode t)

;; go auto complete
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

;;gofmt
(add-hook 'before-save-hook 'gofmt-before-save)

;;set gomode open defaultly
(require 'go-mode-autoloads)

;;go remove unused import
(add-hook 'go-mode-hook '(lambda ()
                           (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))

;;go flymake check
(require 'go-flymake)

;;remove toolbar
(tool-bar-mode 0)

;;auto paire
(require 'autopair)
(autopair-global-mode)

;;hilight pair
(require 'highlight-parentheses)
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (highlight-parentheses-mode)
             (setq autopair-handle-action-fns
                   (list 'autopair-default-handle-action
                         '(lambda (action pair pos-before)
                            (hl-paren-color-update))))))
(add-hook 'highlight-parentheses-mode-hook
          '(lambda ()
             (setq autopair-handle-action-fns
                   (append
                    (if autopair-handle-action-fns
                        autopair-handle-action-fns
                      '(autopair-default-handle-action))
                    '((lambda (action pair pos-before)
                        (hl-paren-color-update)))))))
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;;book mark mode
(require 'bm)

;;set font not mac
(set-default-font "-outline-Consolas-normal-normal-normal-mono-13-*-*-*-c-*-iso8859-1")
;;set chinese not mac
(set-fontset-font "fontset-default" 'gb18030' ("Microsoft YaHei" . "unicode-bmp"))
(when (eq system-type 'darwin)
  ;;mac font
  (set-default-font " -apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
  ;;set mac chinese italic
  (set-fontset-font "fontset-default" 'gb18030' ("STHeiti" . "unicode-bmp"))
  )
;;set offset 4space offset of c mode
(setq c-basic-offset 4)

;;set default tab 4space not tab
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)

;;line mode
(global-linum-mode t)
;;column mode
(column-number-mode t)

;;comment block
(defun my-comment-or-uncomment-region (beg end &optional arg)  
  (interactive (if (use-region-p)  
                   (list (region-beginning) (region-end) nil)  
                 (list (line-beginning-position)  
                       (line-beginning-position 2))))  
  (comment-or-uncomment-region beg end arg)  
  )  

;;set Upper character make sence while search
(setq-default case-fold-search nil)

;;set highlight search result
(setq search-highlight t)

;;set replace highlite
(setq query-replace-highlight t)

;;set a space after colum nomber
(put 'upcase-region 'disabled nil)

;;()pare
(show-paren-mode t)  
(setq show-paren-style 'parentheses)
;;when mouse meet I
(mouse-avoidance-mode 'animate)
;;use I instand of block
(setq-default cursor-type 'bar)

;;set mode alist
(
 setq auto-mode-alist
      ( append 
        '(("\\.ejs\\'" . html-mode)
          ("\\.s?html?\\'" . html-mode)
          (" \\.asp\\'" . html-helper-mode)
          ("\\.phtml\\'" . html-helper-mode)
          ("\\.css\\'" . css-mode)
          ("\\.css\\'" . rainbow-mode)
          ("\\.pc$" . c-mode)
          ("\\.txt\\'" . org-mode)
          ("\\.less\\'" . css-mode)
          )
        auto-mode-alist))

;;set something of speedbar
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-width 20)
(setq speedbar-show-unknown-files t)

;;set alpha
(setq alpha-list '((50 30) (100 100))) 
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab))))
     (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))))

;;set execute
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(put 'set-goal-column 'disabled nil)

;;set environment
(setenv "GOPATH" "/Users/lulala/go")
(setenv "PATH" "/Users/lulala/go/bin:/usr/local/mysql/bin:/Library/Frameworks/Python.framework/Versions/3.4/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/local/git/bin:/usr/local/go/bin:/opt/oracle/instantclient_11_2")


;;hot keys
;;() paring
(global-set-key [(meta left)] 'backward-sexp)
(global-set-key [(meta right)] 'forward-sexp)
;;goto line
(global-set-key [(meta g)] 'goto-line)
;;command block
(global-set-key [(ctrl /)] 'my-comment-or-uncomment-region)
;;set meta real meta
(setq alt 'meta)
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'control)
  ;;fn-delete delete next one
  (global-set-key [kp-delete] 'delete-char)
  )
;;book marker setter & finder
(global-set-key [(control \`)] 'bm-toggle)
(global-set-key [(meta \`)]  'bm-next)
;;replace-string by string or by regexp
(global-set-key (kbd"<f2>") 'replace-string)
(global-set-key (kbd"<f3>") 'replace-regexp)
;;speedbar toggle
(global-set-key(kbd"<f5>")(lambda()
                            (interactive)
                            (sr-speedbar-toggle)
                            ))
;;tabbar
(global-set-key [(meta j)] 'tabbar-backward)  
(global-set-key [(meta k)] 'tabbar-forward) 
;;window switch
(defun nb ()
  (interactive) 
  (other-window 1)
  )
(defun pb()
  (interactive) 
  (other-window -1)
  )
(global-set-key [(meta \[)] 'pb)
(global-set-key [(meta \])] 'nb)
;;loop-alpha
(global-set-key(kbd"<f6>") 'loop-alpha)
