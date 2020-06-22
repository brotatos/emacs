;; Use java style indentation for c/c++
(defun foouser-c++-indent-setup ()
  (c-set-style "java")
  (c-set-offset (quote innamespace) 0 nil))

(add-hook 'c++-mode-hook 'foouser-c++-indent-setup)
