(setq ft:templates:feature
      (make-ft:parameterized-template
       :name "Feature"
       :node (make-ft:directory-node
              :get-name (lambda (template-instance-name) (format "%s" (s-upper-camel-case template-instance-name)))
              :children (list
                      (make-ft:file-node
                       :get-name (lambda (template-instance-name) "index.tsx")
                       :content (make-ft:snippet-content :name "ss-feature-index" :mode 'typescript-mode))
                      (make-ft:file-node
                       :get-name (lambda (template-instance-name) "Component.tsx")
                       :content (make-ft:snippet-content :name "ss-comp" :mode 'typescript-mode))
                      (make-ft:file-node
                       :get-name (lambda (template-instance-name) "style.scss")
                       :content (make-ft:snippet-content :name "style" :mode 'scss-mode))
                      (make-ft:file-node
                       :get-name (lambda (template-instance-name) "i18n.json")
                       :content (make-ft:snippet-content :name "ss-feature-i18n" :mode 'js-mode))))))
