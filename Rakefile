task :default => "test:all"

namespace :test do
  desc "Run tests for Emacs Lisp"
  task :elisp do
    sh(%q[emacs --batch -Q -l tests.el -f ert-run-tests-batch-and-exit]){}
  end

  desc "Run tests for Emacs Lisp interactively"
  task :elispi do
    sh(%q[emacs -Q -l tests.el -eval "(ert-run-tests-interactively 't)"]){}
  end

  desc "Run test:ruby and test:elisp"
  task :all => [:elisp]
end
