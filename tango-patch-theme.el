(deftheme tango-patch
  "Face colors using the Tango palette (light background).
Basic, Font Lock, Isearch, Gnus, Message, Ediff, Flyspell,
Semantic, and Ansi-Color faces are included.")

(custom-theme-set-faces
 'tango-patch
 '(font-lock-comment-face ((t (:foreground "#8f5902"))))
 '(font-lock-constant-face ((t (:foreground "#b35000")))))

(provide-theme 'tango-patch)
