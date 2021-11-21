(setq ft:templates:page
      (make-ft:template
       :name "Page"
       :node (make-ft:directory-node
              :get-name (lambda (template-instance-name) (format "%s" (s-upper-camel-case template-instance-name)))
              :children (list
                      (make-ft:file-node
                       :get-name (lambda (template-instance-name) "index.tsx")
                       :content (make-ft:snippet-content :name "ss-page-index" :mode 'typescript-mode))
                      (make-ft:file-node
                       :get-name (lambda (template-instance-name) "features.tsx")
                       :content (make-ft:snippet-content :name "ss-page-features" :mode 'typescript-mode))))))
