;;;;;;;;;;;;;;;
;; load path ;;
;;;;;;;;;;;;;;;

;; public
(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(add-to-list 'load-path "~/.emacs.d/plugins/theme")
(add-to-list 'load-path "~/.emacs.d/cl-lib/")
(add-to-list 'load-path "~/.emacs.d/plugins/gofly/")
(add-to-list 'load-path "~/")

( if (display-graphic-p)
    ;; gui
    (progn
      ;; init package of emacs
      (when (>= emacs-major-version 24)
        (require 'package)
        (package-initialize)
        (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
        )
      ;; load el package
      (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
      (unless (require 'el-get nil 'noerror)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
      (el-get 'sync)
      )
  ;; commond-line
  (progn
   )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;
;; requirement ;;
;;;;;;;;;;;;;;;;;

;;public
(require 'cl-lib)
(require 'color-theme)
(require 'tabbar)
(require 'go-autocomplete)
(require 'auto-complete-config)
(require 'autopair)
(require 'highlight-parentheses)
(require 'bm)
(require 'go-mode-autoloads)
(require 'exec-path-from-shell)
(require 'go-flymake)

(if (display-graphic-p)
    ;; gui
    (progn
      (require 'yasnippet)
     )
  ;; command line
  (progn
   )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;; init ;;
;;;;;;;;;;

;; public
(load "~/.emacs-cmd")
;;set path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  ;;this make godef goimport can load pkg from GOPATH
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "PATH")
  )

(if (display-graphic-p)
    ;; gui
    (progn
      (color-theme-initialize)
      (color-theme-subtle-hacker)
      ;; powerline-mode
      (setq sml/no-confirm-load-theme t)
      (setq sml/theme 'powerline)
      (smart-mode-line-enable t)
      (setq yas-snippet-dirs
            '("~/.emacs.d/plugins/yasnippet/snippets"
              ))
      (yas-global-mode 1)
      ;; other system except darwin
      (set-default-font "-outline-Consolas-normal-normal-normal-mono-13-*-*-*-c-*-iso8859-1")
      ;; chinese font of other system except darwin
      (set-fontset-font "fontset-default" 'gb18030' ("Microsoft YaHei" . "unicode-bmp"))
      ;; font for darwin 
      (when (eq system-type 'darwin)
        (set-default-font " -apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
        ;; font for Italics chinese
        (set-fontset-font "fontset-default" 'gb18030' ("STHeiti" . "unicode-bmp"))
        )
      )
  ;; command line
  (progn
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; function ;;
;;;;;;;;;;;;;;

;; comment region
(defun my-comment-or-uncomment-region (beg end &optional arg)  
  (interactive (if (use-region-p)  
                   (list (region-beginning) (region-end) nil)  
                 (list (line-beginning-position)  
                       (line-beginning-position 2))))  
  (comment-or-uncomment-region beg end arg)  
  )  


;; buferjunmping
;; next-buffer
(defun nb ()
  (interactive) 
  (other-window 1)
  )
;; previous-buffer
(defun pb()
  (interactive) 
  (other-window -1)
  )

;; set loop-alpha for the window
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab))))
     (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))))

;; define go-mode-hook
(defun my-go-mode-hook ()
  ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;

;; global
;; set default coding system
(setq default-buffer-file-coding-system 'utf-8-unix)
;; close welcome page
(setq inhibit-startup-message t)
;; set default c offset
(setq c-basic-offset 4)
;; change default tab witdh to 4 space
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
;; set column open
(column-number-mode t)
;; highlight all when serching
(setq search-highlight t)
;; hilight all when replaceing
(setq query-replace-highlight t)
;; open paren mode
(show-paren-mode t)  
(setq show-paren-style 'parentheses)
;; able all commond in emacs Orz...
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'erase-buffer 'disabled nil)
;; when cursor meet mouse move mouse away Orz...
(mouse-avoidance-mode 'animate)
;;change cursor type
(setq-default cursor-type 'bar)
;; set window alpha-list
(setq alpha-list '((50 30) (100 100)))
;; enable mouse auto select buffer
(setq mouse-autoselect-window t)
;; set default shell bash
(setq-default shell-file-name "/bin/bash")
;;enable tabbar-mode
(tabbar-mode 1)
;; auto complete
(ac-config-default)
(auto-complete-mode t)
;; disable tool bar
(autopair-global-mode)

( if (display-graphic-p)
    ;; set linum open
    (progn
      (global-linum-mode t)
      (tool-bar-mode 0)
    )
)



;; custome things of emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; modes
;; self-define modes
;; change protobuf mode offset 2
(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))

;; define hilight parentheses mode
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; append mode list
(
 setq auto-mode-alist
      ( append 
        '(("\\.ejs\\'" . html-mode)
          ("\\.s?html?\\'" . html-mode)
          (" \\.asp\\'" . html-helper-mode)
          ("\\.phtml\\'" . html-helper-mode)
          ("\\.css\\'" . css-mode)
          ("\\.css\\'" . rainbow-mode)
          ("\\.coffee\\'" . coffee-mode)
          ("\\.pc$" . c-mode)
          ("\\.txt\\'" . org-mode)
          ("\\.less\\'" . css-mode)
          )
        auto-mode-alist))
;; enable org mode export to markdown
(eval-after-load "org"
  '(require 'ox-md nil t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;
;; hotkeys ;;
;;;;;;;;;;;;;

;; set alt meta
(setq alt 'meta)

;; when using darwin
;; set command as control
;; set meta meta
;; set controle command
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'control)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )
;; for bracket matching
(global-set-key [(meta left)] 'backward-sexp)
(global-set-key [(meta right)] 'forward-sexp)
;;goto line
(global-set-key [(meta g)] 'goto-line)
;;comment region
(global-set-key [(control /)] 'my-comment-or-uncomment-region)
;;toggle bookmark and bookmark loop
(global-set-key [(ctrl \`)] 'bm-toggle)
(global-set-key [(meta \`)]  'bm-next)
;;设置replace-string
(global-set-key (kbd"<f2>") 'replace-string)
(global-set-key (kbd"<f3>") 'replace-regexp)
(global-set-key (kbd"<f4>") 'search-forward-regexp)
;;tabbar loop
(global-set-key [(meta j)] 'tabbar-backward)  
(global-set-key [(meta k)] 'tabbar-forward) 
;;buffer loop
(global-set-key [(meta \[)] 'pb)  
(global-set-key [(meta \])] 'nb)
;;window alpha loop
(global-set-key(kbd"<f6>") 'loop-alpha)
;;code flod
(global-set-key (kbd "C-c C-f") 'fold-this-all)
(global-set-key (kbd "C-c C-F") 'fold-this)
(global-set-key (kbd "C-c C-v") 'fold-this-unfold-all)
;;rgrep
(global-set-key (kbd"<f8>") 'rgrep)

;;something for golang
;;go fly-make go to prev/next error golang
(global-set-key (kbd "C-c C-n") 'flymake-goto-prev-error)
;;pop-tag-mark when using c-j trigger godef
(global-set-key (kbd "C-c C-b") 'pop-tag-mark)
;;add hot key when i need use mouse Orz....
(global-set-key (kbd "C-S-<mouse-1>") 'godef-jump)
(global-set-key (kbd "C-S-<mouse-3>") 'pop-tag-mark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;
;; hooks ;;
;;;;;;;;;;;

;; lisp mode hook
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (highlight-parentheses-mode)
             (setq autopair-handle-action-fns
                   (list 'autopair-default-handle-action
                         '(lambda (action pair pos-before)
                            (hl-paren-color-update))))))
;; highlight parentheses mode hook
(add-hook 'highlight-parentheses-mode-hook
          '(lambda ()
             (setq autopair-handle-action-fns
                   (append
                    (if autopair-handle-action-fns
                        autopair-handle-action-fns
                      '(autopair-default-handle-action))
                    '((lambda (action pair pos-before)
                        (hl-paren-color-update)))))))
                            
;; protobuf offset hook
(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "my-style" my-protobuf-style t)))

;; stylus-mode-hook
(add-hook 'stylus-mode-hook
          '(lambda ()
             (yas-minor-mode -1)
             (rainbow-mode)
             ))
;; go mode hook
(add-hook 'go-mode-hook 'my-go-mode-hook)
