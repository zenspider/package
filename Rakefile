task :default => "test:all"

def run cmd
  sh cmd do
    # block prevents ruby backtrace on failure
  end
end

def emacs args
  emacs_cmd = Dir[
    "/{My,}Applications/Emacs.app/Contents/MacOS/Emacs", # cask installs
    "/opt/homebrew/bin/emacs",                           # M1 homebrew
    "/usr/local/bin/emacs",                              # old homebrew
    "/usr/bin/emacs",                                    # linux
  ].first || "emacs" # trust the path

  run %Q[#{emacs_cmd} -Q -L . #{args}]
end

def emacs_test args
  emacs "-l tests.el #{args}"
end

el_files = Rake::FileList['**/*.el']

task compile: el_files.ext('.elc')

desc "byte compile the project. Helps drive out warnings, but also faster."
rule '.elc' => '.el' do |t|
  emacs "--batch -f batch-byte-compile #{t.source}"
end

desc "Clean the project"
task :clean do
  rm_f Dir["**/*~", "**/*.elc"]
end

task :test => "test:all"

namespace :test do
  desc "Run tests for Emacs Lisp"
  task :elisp do
    n=ENV["N"]

    if n then
      emacs_test "--batch -eval '(ert-run-tests-batch-and-exit #{n.dump})'"
    else
      emacs_test "--batch -f ert-run-tests-batch-and-exit"
    end
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
