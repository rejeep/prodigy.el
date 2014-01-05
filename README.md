# Prodigy [![Build Status](https://api.travis-ci.org/rejeep/prodigy.el.png?branch=master)](http://travis-ci.org/rejeep/prodigy.el)

Manage external services from within Emacs

I came up with the idea when I got to work one Monday morning and
before I could start working I had to manually start ten or so
services.

To get rid of this tedious work, I started working on this Emacs
plugin. Prodigy provides a
[Magit](https://github.com/magit/magit)-like GUI to manage services in
a simple way.

![Prodigy](/prodigy.png)

## Installation

Add `prodigy` to your [Cask](https://github.com/cask/cask) file:

```lisp
(depends-on "prodigy")
```

## Usage

Start Prodigy with `M-x prodigy`. You should see a list of all defined
services.

### Services

Services can be defined in a few different ways. See doc-string for
information about available properties to specify: `M-x
describe-variable RET prodigy-services`.

#### prodigy-define-service (`&rest args`)

Services can be defined using the function `prodigy-define-service`:

```lisp
(prodigy-define-service :prop value ...)
```

#### prodigy-services

Services can be defined by setting the variable `prodigy-services`:

```lisp
(setq prodigy-services
 '((:prop value ...)
   (:prop value ...)))
```

### Tags

Services can have any number of tags. Tags does not have to be pre
defined. If they are, the service will inherit all the tags
properties. See doc-string for information about available properties
to specify: `M-x describe-variable RET prodigy-tags`.

#### prodigy-define-tag (`&rest args`)

Tags can be defined using the function `prodigy-define-tag`:

```lisp
(prodigy-define-tag :prop value ...)
```

#### prodigy-tags

Tags can be defined by setting the variable `prodigy-tags`:

```lisp
(setq prodigy-tags
 '((:prop value ...)
   (:prop value ...)))
```

### Filters

Filters is a way to show only specific services in the Prodigy
buffer. For example services with specific tag or with a name matching
a string.

To add a filter, use `prodigy-add-filter`:

```lisp
(prodigy-add-filter :tag 'foo)
(prodigy-add-filter :name "bar")
```

You can also set the variable `prodigy-filters` directly:

```lisp
(setq prodigy-filters
      '((:tag foo)
        (:name "bar")))
```

## Examples

Start simple Python server:

```lisp
(prodigy-define-service
  :name "Python app"
  :command "python"
  :cwd "/path/to/my/project"
  :args '("-m" "SimpleHTTPServer" "6001")
  :tags '(work)
  :kill-signal 'sigint
  :kill-process-buffer-on-stop t)
```

Start Node server:

```lisp
(prodigy-define-service
  :name "Node app"
  :command "nodemon"
  :cwd "/path/to/my/project"
  :args '("app.coffee")
  :port 6002
  :tags '(work node)
  :init-async (lambda (done)
                (nvm-use-for "/path/to/my/project" done)))
```

Start Sinatra server:

```lisp
(prodigy-define-service
  :name "Sinatra"
  :command "server"
  :cwd "/path/to/my/project"
  :path '("/path/to/my/project/bin")
  :port 6003
  :tags '(work ruby)
  :init-async (lambda (done)
                (rvm-activate-ruby-for "/path/to/my/project" done)))
```

Using tags you can avoid repeating common tasks such as setting up Bundler:

```lisp
(prodigy-define-tag
  :name 'rvm
  :init-async (lambda (done)
                (rvm-activate-ruby-for default-directory done)))

(prodigy-define-service
  :name "Rails"
  :command "bundle"
  :args '("exec" "rails" "server")
  :cwd "/path/to/my/project"
  :tags '(rvm))

(prodigy-define-service
  :name "Sinatra"
  :command "bundle"
  :args '("exec" "rackup")
  :cwd "/path/to/my/project"
  :tags '(rvm))
```

## Troubleshoot

### Jekyll

For some unknown reason, Jekyll fail with this error:

```
error: invalid byte sequence in US-ASCII. Use --trace to view backtrace
```

This can be solved by adding a `jekyll` tag, like this:

```lisp
(prodigy-define-tag
  :name 'jekyll
  :env '(("LANG" "en_US.UTF-8")
         ("LC_ALL" "en_US.UTF-8")))
```

Then tag your services with the `jekyll` tag.

## Contribution

Contribution is much welcome!

Install [Cask](https://github.com/cask/cask) if you haven't
already, then:

    $ cd /path/to/prodigy.el
    $ cask

Run all tests with:

    $ make
