( if (display-graphic-p)
    (progn
      (when (>= emacs-major-version 24)
        (require 'package)
        (package-initialize)
        (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
        )
      (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
      (unless (require 'el-get nil 'noerror)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
          (goto-char (point-max))
          (eval-print-last-sexp)))

      (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
      (el-get 'sync)
      ;;设置loadpath
      (add-to-list 'load-path "~/.emacs.d/plugins/")
      (add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
      (add-to-list 'load-path "~/.emacs.d/plugins/theme")
      (add-to-list 'load-path "~/.emacs.d/cl-lib/")
      (add-to-list 'load-path "~/.emacs.d/plugins/gofly/")
      (require 'cl-lib)
      ;;设置为默认utf-8
      (setq default-buffer-file-coding-system 'utf-8-unix)
      ;;关闭欢迎界面
      (setq inhibit-startup-message t)
      ;;主题
      (require 'color-theme)
      (color-theme-initialize)
      (color-theme-subtle-hacker)
      ;;模板
      (require 'yasnippet)

      (setq yas-snippet-dirs
            '("~/.emacs.d/plugins/yasnippet/snippets"
              ))
      (yas-global-mode 1)


      ;;tabbar
      (require 'tabbar)
      (tabbar-mode 1)

      ;;go 自动补全
      (require 'go-autocomplete)
      ;;自动补全
      (require 'auto-complete-config)
      (ac-config-default)
      (auto-complete-mode t)

      ;;gofmt
      ;; (add-hook 'after-save-hook 'go-remove-unused-imports)
      ;; (add-hook 'before-save-hook 'gofmt-before-save)

      (tool-bar-mode 0)

      ;;自动补全括号
      (require 'autopair)
      (autopair-global-mode)
      ;;高亮括号
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

      ;;设置bookmark
      (require 'bm)

      ;;设置非macOS的字体
      (set-default-font "-outline-Consolas-normal-normal-normal-mono-13-*-*-*-c-*-iso8859-1")
      ;;非mac的中文字体
      (set-fontset-font "fontset-default" 'gb18030' ("Microsoft YaHei" . "unicode-bmp"))
      (when (eq system-type 'darwin)
        ;;设置一个MacOS字体
        (set-default-font " -apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
        ;;干掉小方框！！因为上面那个万恶的字体没有草泥马的斜体！！！
        (set-fontset-font "fontset-default" 'gb18030' ("STHeiti" . "unicode-bmp"))
        )
      ;;设置一个屌逼的c风格的缩进
      (setq c-basic-offset 4)
      ;;关掉默认的tab设置为4个空格
      (setq default-tab-width 4)
      (setq-default indent-tabs-mode nil)
      ;;弄出来行号
      (global-linum-mode t)
      ;;列号
      (column-number-mode t)
      ;;设置好左右匹配
      (global-set-key [(meta left)] 'backward-sexp)
      (global-set-key [(meta right)] 'forward-sexp)
      ;;M-g设置为gotoline
      (global-set-key [(meta g)] 'goto-line)
      ;;设置注释区域
      (defun my-comment-or-uncomment-region (beg end &optional arg)  
        (interactive (if (use-region-p)  
                         (list (region-beginning) (region-end) nil)  
                       (list (line-beginning-position)  
                             (line-beginning-position 2))))  
        (comment-or-uncomment-region beg end arg)  
        )  
      (global-set-key [(control /)] 'my-comment-or-uncomment-region)

      ;;设置搜索时严格按照大小写
      ;; (setq-default case-fold-search nil)

      ;;设置搜索时高亮系那是全部
      (setq search-highlight t)

      ;;设置替换时高亮显示
      (setq query-replace-highlight t)
      ;;设置背景颜色
      ;;(set-background-color "#faf9de")
      ;;应该是行号之后空一条
      (put 'upcase-region 'disabled nil)

      ;;set meta real meta
      (setq alt 'meta)
      (when (eq system-type 'darwin)
        (setq mac-option-modifier 'meta)
        (setq mac-command-modifier 'control)
        (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
        )
      ;;()的匹配
      (show-paren-mode t)  
      (setq show-paren-style 'parentheses)
      ;;光标移动到鼠标的时候把鼠标干掉！
      (mouse-avoidance-mode 'animate)
      ;;设置光标为|而不是小黑块
      (setq-default cursor-type 'bar)
      ;;设置打开ejs的时候默认使用html-mode
      (
       setq auto-mode-alist
            ( append 
              '(("\\.ejs\\'" . html-mode)
                ("\\.s?html?\\'" . html-mode)
                (" \\.asp\\'" . html-helper-mode)
                ("\\.phtml\\'" . html-helper-mode)
                ("\\.css\\'" . css-mode)
                ("\\.pc$" . c-mode)
                )
              auto-mode-alist))

      ;;设置使用org模式打开.txt Orz....
      (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
      ;;设置less文件用css方式打开
      (add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode
                                      ))
      ;;设置gomode 自动打开
      (require 'go-mode-autoloads)

      ;;设置bookmark的快捷键
      (global-set-key [(ctrl \`)] 'bm-toggle)
      (global-set-key [(meta \`)]  'bm-next)


      ;;设置replace-string
      (global-set-key (kbd"<f2>") 'replace-string)
      (global-set-key (kbd"<f3>") 'replace-regexp)
      (global-set-key (kbd"<f4>") 'search-forward-regexp)

      ;;tabbar hot key
      (global-set-key [(meta j)] 'tabbar-backward)  
      (global-set-key [(meta k)] 'tabbar-forward) 

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

      ;;shell config
      ;; (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t) 
      ;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

      ;;半透明
      (global-set-key(kbd"<f6>") 'loop-alpha) ;;全局绑定F6键为Emacs半透明功能键
      (setq alpha-list '((50 30) (100 100))) ;;当前窗口和非当前窗口时透明度分别为85、50
      (defun loop-alpha ()
        (interactive)
        (let ((h (car alpha-list)))
          ((lambda (a ab)
             (set-frame-parameter (selected-frame) 'alpha (list a ab))
             (add-to-list 'default-frame-alist (cons 'alpha (list a ab))))
           (car h) (car (cdr h)))
          (setq alpha-list (cdr (append alpha-list (list h))))))

      (require 'exec-path-from-shell)
      (when (memq window-system '(mac ns))
        (exec-path-from-shell-initialize)
        ;;this make godef goimport can load pkg from GOPATH
        (exec-path-from-shell-copy-env "GOPATH")
        (exec-path-from-shell-copy-env "PATH")
        )

      (require 'go-flymake)

      ;; goimports
      (defun my-go-mode-hook ()
                                        ; Use goimports instead of go-fmt
        (setq gofmt-command "goimports")
                                        ; Call Gofmt before saving
        (add-hook 'before-save-hook 'gofmt-before-save)
                                        ; Customize compile command to run go build
        (if (not (string-match "go" compile-command))
            (set (make-local-variable 'compile-command)
                 "go build -v && go test -v && go vet"))
                                        ; Godef jump key binding
        (local-set-key (kbd "M-.") 'godef-jump))
      (add-hook 'go-mode-hook 'my-go-mode-hook)
      (setq c-basic-offset 4)

      ;; proto buf mode offset
      (defconst my-protobuf-style
        '((c-basic-offset . 2)
          (indent-tabs-mode . nil)))
      (add-hook 'protobuf-mode-hook
                (lambda () (c-add-style "my-style" my-protobuf-style t)))

      (global-set-key (kbd "C-c C-f") 'fold-this-all)
      (global-set-key (kbd "C-c C-F") 'fold-this)
      (global-set-key (kbd "C-c C-v") 'fold-this-unfold-all)
      ;;go to prev/next error golang
      (global-set-key (kbd "C-c C-n") 'flymake-goto-next-error)
      (add-to-list 'load-path "~/")
      (load "~/.emacs-cmd")

      (put 'downcase-region 'disabled nil)



      (add-hook 'stylus-mode-hook
                '(lambda ()
                   (yas-minor-mode -1)
                   (rainbow-mode)
                   ))
      (require 'jsfmt)
      (add-hook 'before-save-hook 'jsfmt-before-save)
      )
  (progn
    ;;设置loadpath
    (add-to-list 'load-path "~/.emacs.d/plugins/")
    (add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
    (add-to-list 'load-path "~/.emacs.d/plugins/theme")
    (add-to-list 'load-path "~/.emacs.d/cl-lib/")
    (add-to-list 'load-path "~/.emacs.d/plugins/gofly/")
    (require 'cl-lib)
    ;;设置为默认utf-8
    (setq default-buffer-file-coding-system 'utf-8-unix)
    ;;关闭欢迎界面
    (setq inhibit-startup-message t)
    ;;主题
    ;;(require 'color-theme)
    ;;(color-theme-initialize)

    ;;tabbar
    (require 'tabbar)
    (tabbar-mode 1)

    ;;go 自动补全
    (require 'go-autocomplete)
    ;;自动补全
    (require 'auto-complete-config)
    (ac-config-default)
    (auto-complete-mode t)

    ;;自动补全括号
    (require 'autopair)
    (autopair-global-mode)
    ;;高亮括号
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

    ;;设置bookmark
    (require 'bm)

    ;;设置非macOS的字体
    (set-default-font "-outline-Consolas-normal-normal-normal-mono-13-*-*-*-c-*-iso8859-1")
    ;;非mac的中文字体
    (when (eq system-type 'darwin)
      ;;设置一个MacOS字体
      (set-default-font " -apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
      )
    ;;设置一个屌逼的c风格的缩进
    (setq c-basic-offset 4)
    ;;关掉默认的tab设置为4个空格
    (setq default-tab-width 4)
    (setq-default indent-tabs-mode nil)
    ;;列号
    (column-number-mode t)
    ;;设置好左右匹配
    (global-set-key [(meta left)] 'backward-sexp)
    (global-set-key [(meta right)] 'forward-sexp)
    ;;M-g设置为gotoline
    (global-set-key [(meta g)] 'goto-line)
    ;;设置注释区域
    (defun my-comment-or-uncomment-region (beg end &optional arg)  
      (interactive (if (use-region-p)  
                       (list (region-beginning) (region-end) nil)  
                     (list (line-beginning-position)  
                           (line-beginning-position 2))))  
      (comment-or-uncomment-region beg end arg)  
      )  
    (global-set-key [(control /)] 'my-comment-or-uncomment-region)

    ;;设置搜索时严格按照大小写
    ;; (setq-default case-fold-search nil)

    ;;设置搜索时高亮系那是全部
    (setq search-highlight t)

    ;;设置替换时高亮显示
    (setq query-replace-highlight t)
    ;;设置背景颜色
    ;;(set-background-color "#faf9de")
    ;;应该是行号之后空一条
    (put 'upcase-region 'disabled nil)

    ;;set meta real meta
    (setq alt 'meta)
    (when (eq system-type 'darwin)
      (setq mac-option-modifier 'meta)
      (setq mac-command-modifier 'control)
      (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
      )
    ;;()的匹配
    (show-paren-mode t)  
    (setq show-paren-style 'parentheses)
    ;;光标移动到鼠标的时候把鼠标干掉！
    (mouse-avoidance-mode 'animate)
    ;;设置光标为|而不是小黑块
    (setq-default cursor-type 'bar)
    ;;设置打开ejs的时候默认使用html-mode
    (
     setq auto-mode-alist
          ( append 
            '(("\\.ejs\\'" . html-mode)
              ("\\.s?html?\\'" . html-mode)
              (" \\.asp\\'" . html-helper-mode)
              ("\\.phtml\\'" . html-helper-mode)
              ("\\.css\\'" . css-mode)
              ("\\.pc$" . c-mode)
              )
            auto-mode-alist))

    ;;设置使用org模式打开.txt Orz....
    (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
    ;;设置less文件用css方式打开
    (add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode
                                    ))
    ;;设置gomode 自动打开
    (require 'go-mode-autoloads)

    ;;设置bookmark的快捷键
    (global-set-key [(ctrl \`)] 'bm-toggle)
    (global-set-key [(meta \`)]  'bm-next)


    ;;设置replace-string
    (global-set-key (kbd"<f2>") 'replace-string)
    (global-set-key (kbd"<f3>") 'replace-regexp)
    (global-set-key (kbd"<f4>") 'search-forward-regexp)

    ;;tabbar hot key
    (global-set-key [(meta j)] 'tabbar-backward)  
    (global-set-key [(meta k)] 'tabbar-forward) 

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

    ;;shell config
    ;; (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t) 
    ;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

    ;;半透明
    (global-set-key(kbd"<f6>") 'loop-alpha) ;;全局绑定F6键为Emacs半透明功能键
    (setq alpha-list '((50 30) (100 100))) ;;当前窗口和非当前窗口时透明度分别为85、50
    (defun loop-alpha ()
      (interactive)
      (let ((h (car alpha-list)))
        ((lambda (a ab)
           (set-frame-parameter (selected-frame) 'alpha (list a ab))
           (add-to-list 'default-frame-alist (cons 'alpha (list a ab))))
         (car h) (car (cdr h)))
        (setq alpha-list (cdr (append alpha-list (list h))))))

    (require 'exec-path-from-shell)
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize)
      ;;this make godef goimport can load pkg from GOPATH
      (exec-path-from-shell-copy-env "GOPATH")
      (exec-path-from-shell-copy-env "PATH")
      )

    (require 'go-flymake)

    ;; goimports
    (defun my-go-mode-hook ()
                                        ; Use goimports instead of go-fmt
      (setq gofmt-command "goimports")
                                        ; Call Gofmt before saving
      (add-hook 'before-save-hook 'gofmt-before-save)
                                        ; Customize compile command to run go build
      (if (not (string-match "go" compile-command))
          (set (make-local-variable 'compile-command)
               "go build -v && go test -v && go vet"))
                                        ; Godef jump key binding
      (local-set-key (kbd "M-.") 'godef-jump))
    (add-hook 'go-mode-hook 'my-go-mode-hook)
    (setq c-basic-offset 4)

    ;; proto buf mode offset
    (defconst my-protobuf-style
      '((c-basic-offset . 2)
        (indent-tabs-mode . nil)))
    (add-hook 'protobuf-mode-hook
              (lambda () (c-add-style "my-style" my-protobuf-style t)))

    (global-set-key (kbd "C-c C-f") 'fold-this-all)
    (global-set-key (kbd "C-c C-F") 'fold-this)
    (global-set-key (kbd "C-c C-v") 'fold-this-unfold-all)
    ;;go to prev/next error golang
    (global-set-key (kbd "C-c C-n") 'flymake-goto-prev-error)
    (global-set-key (kbd "C-c C-n") 'flymake-goto-next-error)
    (add-to-list 'load-path "~/")
    (load "~/.emacs-cmd")

    (put 'downcase-region 'disabled nil)

    (add-hook 'stylus-mode-hook
              '(lambda ()
                 (yas-minor-mode -1)
                 (rainbow-mode)
                 ))
    (require 'jsfmt)
    (add-hook 'before-save-hook 'jsfmt-before-save)
    )
  )

(eval-after-load "org"
  '(require 'ox-md nil t))
(global-set-key (kbd "C-c C-b") 'pop-tag-mark)
(setq mouse-autoselect-window t)
(global-set-key (kbd"<f8>") 'rgrep)
