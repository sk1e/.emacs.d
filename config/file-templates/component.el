(setq ft:templates:component
      (make-ft:parameterized-template
       :name "Component"
       :node (make-ft:directory-node
              :get-name (lambda (template-instance-name) (format "%s" (s-upper-camel-case template-instance-name)))
              :children (list
                      (make-ft:file-node
                       :get-name (lambda (template-instance-name) "index.tsx")
                       :content (make-ft:snippet-content :name "ss-comp" :mode 'typescript-mode))
                      (make-ft:file-node
                       :get-name (lambda (template-instance-name) "style.scss")
                       :content (make-ft:snippet-content :name "style" :mode 'scss-mode))))))
