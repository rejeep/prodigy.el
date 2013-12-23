# Prodigy [![Build Status](https://api.travis-ci.org/rejeep/prodigy.el.png?branch=master)](http://travis-ci.org/rejeep/prodigy.el)

Manage external services from within Emacs

I came up with the idea when I got to work one Monday morning and
before I could start working I had to manually start ten or so
services.

To get rid of this tedious work, I started working on this Emacs
plugin. Prodigy provides a
[Magit](https://github.com/magit/magit)-like GUI to manage services in
a simple way.

## Installation

Add `prodigy` to your [Cask](https://github.com/cask/cask) file:

```lisp
(depends-on "prodigy")
```

## Usage

Services can be defined in a few different ways. See doc-string for
information about available properties to specify: `M-x
describe-variable RET prodigy-services`.

### prodigy-define-service (`&rest args`)

Services can be defined using the function `prodigy-define-service`:

```lisp
(prodigy-define-service :prop value ...)
```

### prodigy-services

Services can be defined by setting the variable `prodigy-services`:

```lisp
(setq prodigy-services
 '((:prop value ...)
   (:prop value ...)))
```

### customize

TODO

## Filters

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

## Commands

Start Prodigy with `M-x prodigy`. You should see a list of all defined
services.

### Quit (`q`)

Quit Prodigy.

### Next service (`n`)

Go to next service.

### Prev service (`p`)

Go to previous service.

### Start service (`s`)

Start service at line or marked services.

### Stop service (`S`)

Stop service at line or marked services.

### Restart service (`r`)

Restart service at line or marked services.

### Display service process output (`$`)

Switch to buffer for service at line.

### Open in browser (`o`)

Open service at line in browser.

### Mark service (`m`)

Mark service at line.

### Mark services with tag (`t`)

Mark services with tag.

### Mark all services (`M`)

Mark all services.

### Unmark service (`u`)

Unmark service at line.

### Unmark services with tag (`t`)

Unmark services with tag.

### Unmark all services (`U`)

Unmark all services.

### Refresh GUI (`g`)

Refresh GUI.

### Add tag filter (`f t`)

Read tag and show only services with that tag.

### Add name filter (`f n`)

Read string and show only services with name that contains string.

### Clear filters (`F`)

Clear all filters.

### Jump - Magit (`j m`)

Jump to Magit.

### Jump - Dired (`j d`)

Jump to Dired.

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
  "My awesome Node app."
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

## Contribution

Contribution is much welcome!

Install [Cask](https://github.com/cask/cask) if you haven't
already, then:

    $ cd /path/to/prodigy.el
    $ cask

Run all tests with:

    $ make
