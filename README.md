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

#### prodigy-add-filter (`&rest args`)

To add a filter, use `prodigy-add-filter`:

```lisp
(prodigy-add-filter :tag 'foo)
(prodigy-add-filter :name "bar")
```

#### prodigy-filters

You can also set the variable `prodigy-filters` directly:

```lisp
(setq prodigy-filters
      '((:tag foo)
        (:name "bar")))
```

### Status

Each service is associated with a status. The built in statuses are:

* `stopped` (default) - The process is not running.
* `running` - The process is running. If the process status is `run`,
  this status will be used.
* `ready` - The process is "actually" ready. Not managed by Prodigy.
* `restarting` - Set when restarting a service.
* `failed` - The process failed. Not managed by Prodigy.

The only way Prodigy has an idea of the service status, is to look at
the process status (note the difference between service and process
status). The process status is however not always a very good
indication of the service "actual" status. For example, it takes about
five seconds to start a Rails server, but the process status will be
`run` almost instantly after started.

To improve the service status, there is a function called
`prodigy-set-status`, that can change the status of a service. The
function takes two arguments: The `service` and the `status-id`. The
status id has to be one of the statuses in `prodigy-status-list`.

You can create your own status with the function
`prodigy-define-status`. See doc-string for information about
available properties to specify: `M-x describe-variable RET
prodigy-status-list`.

For more information, see status example below!

## Examples

Start simple Python server:

```lisp
(prodigy-define-service
  :name "Python app"
  :command "python"
  :cwd "/path/to/my/project"
  :args '("-m" "SimpleHTTPServer" "6001")
  :tags '(work)
  :kill-signal 'sigkill
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

Manually setting status to `ready` when the service is actually ready:

```lisp
(prodigy-define-tag
  :name 'rails
  :on-output (lambda (service output)
               (when (or (s-matches? "Listening on 0\.0\.0\.0:[0-9]+, CTRL\\+C to stop" output)
                         (s-matches? "Ctrl-C to shutdown server" output))
                 (prodigy-set-status service 'ready))))

(prodigy-define-service
  :name "Rails"
  :command "bundle"
  :args '("exec" "rails" "server")
  :cwd "/path/to/my/project"
  :tags '(rvm rails))

(prodigy-define-service
  :name "Sinatra"
  :command "bundle"
  :args '("exec" "rackup")
  :cwd "/path/to/my/project"
  :tags '(rvm rails))
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
