# Prodigy

Manage external processes from Emacs.

TODO: More info here

## Installation

Add `prodigy` to your [Cask](https://github.com/rejeep/prodigy.el) file:

```lisp
(depends-on "prodigy")
```

## API

### prodigy-define-service (`&optional doc-string &rest args`)

Available options:

* `:name` - Name of service
* `:command` - Command to run
* `:args` - Arguments passed to command
* `:cwd` - Run command with this as `default-directory`
* `:port` - Specify service port for use with open function
* `:tags` - List of tags

## Commands

Start Prodigy with `prodigy`. You should see a list of all defined
processes.

### Quit (`q`)

Quit Prodigy.

### Next (`n`)

Go to next service.

### Prev (`p`)

Go to previous service.

### Start (`s`)

Start service at line or marked services.

### Stop (`S`)

Stop service at line or marked services.

### Restart (`r`)

Restart service at line or marked services.

### Switch to buffer (`l`)

Switch to buffer for service at line.

### Open (`o`)

Open service at line in browser.

### Mark (`m`)

Mark service at line.

### Unmark (`u`)

Unmark service at line.

### Mark all (`M`)

Mark all services.

### Unmark all (`U`)

Unmark all services.

## Examples

Start simple Python server:

```lisp
(prodigy-define-service
  :name "Python app"
  :command "python"
  :cwd "/path/to/my/project"
  :args '("-m" "SimpleHTTPServer" "6001")
  :tags '(work))
```

Start nodemon server:

```lisp
(prodigy-define-service
  "My awesome Node app."
  :name "Node app"
  :command "nodemon"
  :cwd "/path/to/my/project"
  :args '("app.coffee")
  :port 6002
  :tags '(work node))
```

## Contribution

Contribution is much welcome!

Install [cask](https://github.com/cask/cask) if you haven't
already, then:

    $ cd /path/to/prodigy.el
    $ cask

Run all tests with:

    $ make
