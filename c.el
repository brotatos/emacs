(setq-default c-basic-offset 2)
(setq-default cpp-basic-offset 2)
(setq c-default-style "k&r")


                                        ; style I want to use in c++ mode
(c-add-style "my-style"
             ;'("stroustrup"
             '("k&r"
               (indent-tabs-mode . nil)        ; use spaces rather than tabs
               (c-basic-offset . 2)            ; indent by four spaces
               (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (brace-list-open . 0)
                                   (statement-case-open . +)))))

(defun my-c++-mode-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)
  (c-toggle-auto-hungry-state 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)
