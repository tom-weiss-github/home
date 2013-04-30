;;
;; Emacs Cedet
;;
;; Author:  Tom Weiss
;; email:   weiss@cs.wisc.edu
;;

;; Installation
;; I had to sudo yum install texinfo in order to get makeinfo.
;; (view) curl http://www.randomsample.de/cedet-snapshots
;; (get) curl -O http://www.randomsample.de/cedet-snapshots/cedet_snapshot-rev-8557.tar.gz

;; make clean-all & make

;; Useful information
;; http://stackoverflow.com/questions/13218664/setting-up-autocomplete-to-work-with-semantic

;; Next: Continue reading through the gentle guide for setting up ede projects.

(load-file "~/Downloads/cedet-bzr/trunk/cedet-devel-load.el")

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)

(semantic-mode 1)

(require 'semantic/ia)
(require 'semantic/bovine/gcc)
(semantic-add-system-include "/opt/gcc-4.7.0/include/c++/4.7.0" 'c++-mode)

