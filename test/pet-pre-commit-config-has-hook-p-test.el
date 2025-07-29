;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-pre-commit-config-has-hook-p"
  :var ((pre-commit-config-content
         '((repos
            ((hooks ((id . "black"))) (repo . "https://github.com/psf/black") (rev . "22.6.0"))))))

  (before-each
    (spy-on 'pet-pre-commit-config :and-return-value pre-commit-config-content))

  (it "should return t if `.pre-commit-config.yaml' has hook declared"
    (expect (pet-pre-commit-config-has-hook-p "black") :to-be-truthy))

  (it "should return nil if `.pre-commit-config.yaml' does not have hook declared"
    (expect (pet-pre-commit-config-has-hook-p "isort") :not :to-be-truthy)))

