(setq ft:templates:i18n-shared-references
      (make-ft:self-sufficient-template
       :name "i18nSharedReferences"
       :node (make-ft:file-node
              :get-name (lambda (template-instance-name) "i18nSharedreferences.ts")
              :content (make-ft:snippet-content :name "ss-i18n-shared-references" :mode 'typescript-mode))))
