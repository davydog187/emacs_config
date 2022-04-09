;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(set-face-attribute 'default nil :font "Fira Code" :height 140)
(setq display-line-numbers-type 'relative)

;; Magit
(setq magit-clone-default-directory "/Users/dave/Development/")
(setq magit-clone-set-remote.pushDefault 't)


(setq visible-bell t)

(add-to-list 'auto-mode-alist '("\\.sface\\'" . html-mode) 'append)

(setq treemacs-sorting 'alphabetic-asc)

(map! :leader :desc "Open Elixir Docs" "oB" (λ! (elixir-mode-open-docs-stable)))


(use-package! alchemist
  :hook (elixir-mode . alchemist-mode)
  :config
  (set-lookup-handlers! 'elixir-mode
    :definition #'alchemist-goto-definition-at-point
    :documentation #'alchemist-help-search-at-point)
  (set-eval-handler! 'elixir-mode #'alchemist-eval-region)
  (set-repl-handler! 'elixir-mode #'alchemist-iex-project-run)
  (setq alchemist-mix-env "dev")
  (setq alchemist-hooks-compile-on-save t)
  (map! :map elixir-mode-map :nv "m" alchemist-mode-keymap))

(use-package! lsp-mode
  :commands lsp
  :hook
  (elixir-mode . lsp))

;; (after! lsp-clients
;;   (lsp-register-client
;;    (make-lsp-client :new-connection
;;     (lsp-stdio-connection
;;         (expand-file-name
;;           "~/Development/elixir-ls/release/language_server.sh"))
;;         :major-modes '(elixir-mode)
;;         :priority -1
;;         :server-id 'elixir-ls
;;         :initialized-fn (lambda (workspace)
;;             (with-lsp-workspace workspace
;;              (let ((config `(:elixirLS
;;                              (:mixEnv "dev"
;;                                      :dialyzerEnabled
;;                                      :json-false))))
;;              (lsp--set-configuration config)))))))

(use-package lsp-mode
  :commands lsp
  :ensure t
  :diminish lsp-mode
  :hook
  (elixir-mode . lsp)
  :init
  (add-to-list 'exec-path "~/.elixir-ls"))


;; lsp-enable-file-watchers
;; lsp-file-watch-threshold

(after! lsp-ui
  (setq lsp-ui-doc-max-height 13
        lsp-ui-doc-max-width 80
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-use-webkit nil
        lsp-ui-flycheck-enable t
        lsp-ui-imenu-kind-position 'left
        lsp-ui-sideline-code-actions-prefix "💡"
        lsp-enable-file-watchers t
        lsp-file-watch-threshold 3000
        ;; fix for completing candidates not showing after “Enum.”:
        company-lsp-match-candidate-predicate #'company-lsp-match-candidate-prefix
        ))


(use-package! dap-mode)

(after! lsp-mode
  (require 'dap-elixir)
  ;;(dap-ui-mode)
  ;;(dap-mode)

    (defun dap-elixir--populate-start-file-args (conf)
    "Populate CONF with the required arguments."
    (-> conf
        (dap--put-if-absent :dap-server-path `("~/.elixir-ls/debugger.sh"))
        (dap--put-if-absent :type "mix_task")
        (dap--put-if-absent :name "mix test")
        (dap--put-if-absent :request "launch")
        (dap--put-if-absent :task "test")
        (dap--put-if-absent :taskArgs (list "--trace"))
        (dap--put-if-absent :projectDir (lsp-find-session-folder (lsp-session) (buffer-file-name)))
        (dap--put-if-absent :cwd (lsp-find-session-folder (lsp-session) (buffer-file-name)))
        (dap--put-if-absent :requireFiles (list
                                            "lib/**"
                                            "test/**/test_helper.exs"
                                            "test/**/*_test.exs"))))

    (dap-register-debug-provider "Elixir" 'dap-elixir--populate-start-file-args)
    (dap-register-debug-template "Elixir Run Configuration"
                                (list :type "Elixir"
                                    :cwd nil
                                    :request "launch"
                                    :program nil
    :name "Elixir::Run"))
  )


(use-package! exunit)

(use-package! flycheck-credo
  :after flycheck
  :config
    (flycheck-credo-setup)
    (after! lsp-ui
      (flycheck-add-next-checker 'lsp-ui 'elixir-credo)))

(after! lsp-mode
  (add-hook 'elixir-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'lsp-format-buffer nil t)
              (add-hook 'after-save-hook 'alchemist-iex-reload-module nil t))))

(after! lsp-mode (setq lsp-file-watch-threshold 2000))

(set-popup-rule! "^\\*Alchemist-IEx" :quit nil :size 0.3)

(map! :mode elixir-mode
      :leader
      :desc "iMenu" :nve  "c/"    #'lsp-ui-imenu
      :desc "Run all tests"   :nve  "tt"   #'exunit-verify-all
      :desc "Test file"   :nve  "tf"   #'alchemist-mix-test-file
      :desc "Re-run tests"   :nve  "tx"   #'exunit-rerun
      :desc "Run single test"   :nve  "ts"   #'exunit-verify-single
      :desc "Run all in umbrella"   :nve  "tT"   #'exunit-verify-all-in-umbrella
      )


;; Eshell function to easily cd to another project
(defun eshell/dev (&optional dir)
  (cd (concat "~/Development/" dir)))

;; (add-to-list 'eshell-visual-commands "docker")
