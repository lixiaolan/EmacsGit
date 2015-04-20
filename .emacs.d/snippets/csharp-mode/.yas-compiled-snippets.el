;;; Compiled snippets and support files for `csharp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'csharp-mode
                     '(("proptrack" "[DataSynchronizationMember]\npublic ${1:string} ${2:MyProperty}\n{\n    get { return this.GetProperty<$1>(\"$2\"); }\n    set { this.SetProperty<$1>(\"$2\", value); }\n}" "public void Method { ... }" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Fri Apr 17 15:16:00 2015
