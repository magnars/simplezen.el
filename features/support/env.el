(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq simplezen-root-path project-directory))

(add-to-list 'load-path simplezen-root-path)

(require 'simplezen)
(require 'espuds)
(require 'ert)

(Before
 (switch-to-buffer
  (get-buffer-create "*simplezen*"))
 (global-set-key (kbd "TAB") 'simplezen-expand)
 (erase-buffer)
 (fundamental-mode)
 (deactivate-mark))

(After)
