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

task :circleci do
  sh "circleci build"
end

task :docker do
  sh "docker run -v $PWD:/package+ -i -t -w /package+ --rm silex/emacs --batch -l tests.el -f ert-run-tests-batch-and-exit"
end

task :dockeri do
  sh "docker run -v $PWD:/package+ -i -t -w /package+ --rm silex/emacs tests.el"
end
