## The clfs Package

The clfs package can simulate all Common Lisp and uiop file
system operations before actually performing them on disk.


It features

* A virtual file system
* Contracts for all file system operations
* A whitelist mechanism for extra safety
* A driver for automated random testing


Some applications are

* Catch errors before they occur
* Limit code access to parts of a disk
* Test code automatically with contracts


The sandbox assumes a single process with complete control over the
accessed files and directories. Transactions or safety for concurrent
processes is beyond the scope of this package.

THE PACKAGE IS STILL EXPERIMENTAL AND UNDER DEVELOPMENT. USE AT YOUR OWN RISK!!!


### Basic Usage

Macro `with-sandbox` runs its body in a sandbox. It expects a
directory as argument, creates a virtual file system, populates it
with the contents of the directory and simulates the actions. If the
body's actions succeed they are executed for real. Consider the
following example.

```
(setf code-dir
      (merge-pathnames "my-project/src/" (user-homedir-pathname)))

(clfs:with-sandbox (code-dir)
  (clfs:delete-file (merge-pathnames "foo.bu" code-dir))
  (clfs:copy-file (merge-pathnames "foo.lisp" code-dir)
                  (merge-pathnames "foo.bu" code-dir)))
```

If file foo.lisp exists this code runs fine, but if it doesn't exist
the script will fail during the copy action.  At that point file
foo.bu was already deleted. This error is caught during the simulation
and file foo.bu on disk will still be there.



### Working with Sandboxes

To work with sandboxes simply replace calls to file system functions
by the shadowed version. For example

`(delete-file "foo.lisp")`

becomes 

`(clfs:delete-file "foo.lisp")`

Normally the shadowed version simply calls the original function, but
in the context of a sandbox it can also call the sandbox version. The
sandbox versions of the file system operations can manipulate files and
directories, but don't store any data. Any data that is written to a file
is disregarded and cannot be read back.


Macro `with-sandbox` sets up a new sandbox containing a virtual file
system. It has various parameters that determine how the actions in its
body behave. The following code shows all parameters and their default
values.

```
(clfs:with-sandbox (code-dir
                    :kind :both    ;; or :execute, or :simulate
                    :test-pre nil
                    :test-post nil
                    :test-diff nil
                    :confine t
                    :close-streams t)
  (clfs:delete-file "foo.lisp"))
```


Option `:kind` can be `:both`, `:execute` or `:simulate`. Default
option `:both` corresponds with the behavior described so far where
the body is first simulated before it is executed. When the option is
`:execute` the body is immediately executed without simulation. This
is equivalent to running the code directlty without `with-sandbox`. If
the option is `:simulate` the body is only simulated and not executed.


The pre-conditions of actions are tested when `:test-pre` has a
non-nil value. An error is thrown when a pre-condition fails.


The post-conditions of actions are tested when `:test-post` has a
non-nil value. An error is thrown when a post-condition fails.


The difference conditions of actions are tested when `:test-diff` has a
non-nil value. An error is thrown when a difference condition
fails. See the section on contracts below for an explanation of the
difference conditions.


The actions are limited to the directory when `:confine` has a non-nil
value. An error is thrown when a file or directory outside the
directory or any of its subdirectories is accessed.


When `:close-streams` has a non-nil value then any stream that was opened
during the evaluation of the sandbox body is closed before leaving the 
body.


The parameters correspond with global variables `*test-pre*`,
`*test-post*`, `*test-diff*` and `*confine*`. The macro binds the
global variables and all actions and observers react to them. The
sandbox is bound to variable `*sandbox*`. Whether code is simulated or
executed is controlled by variable `*simulate*`.  To limit access the
macro adds the directory to global `*whitelist*`.



### Limiting Disk Access

Access to disk is limited by the list of pathnames in special variable
`*whitelist*`. Every shadowed function checks every access to disk
against this whitelist and throws an error if a path is not a subpath
of one of `*whitelist*`'s paths.

Macro `with-sandbox` adds the directory that was passed to it to the
whitelist. For performance and security reasons it is best to limit 
access to directories as deep as possible. Extra directories can always
be added locally by nested use of `with-sandbox`. For example in the
following code function `copy-file` has access to `dir-a` and `dir-b`.

```
(clfs:with-sandbox (dir-a)
  (clfs:with-sandbox (dir-b)
    (clfs:copy-file (merge-pathnames "foo.lisp" dir-a)
                    (merge-pathnames "foo.lisp" dir-b)))
```


### Making functions sandboxable

A function that depends on file contents cannot be run in a sandbox. To make 
such a function suitable for use in a sandbox function `clfs:execute-p` (or 
its opposite `clfs:simulate-p`) can be used to skip the offending reading or
writing.

The virtual file system can simulate file system manipulations but
since it does not store any file contents it cannot do any
reading. Writing is supported, but the written data is simply
disregarded and cannot be read back. This means that actions cannot be
simulated if they depend on the contents of the file system. For some
situations a useful pattern is to make a plan outside the sandbox and
next run that plan inside a sandbox. But for functions that read file
contents this is not sufficient. To make such functions sandboxable the
offending behaviour needs to be avoided.

Consider the following function `convert-file` that reads the contents 
of file `in`, converts it and writes the result in file `out` if necessary.
This function cannot be called in a sandbox because it reads from a file.

```
(defun convert-file (in out)
  (when (dirty-p in out)
    (with-open-file (s out
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (write-converted (read-contents in) s))))
```

The question is how to make the function sandboxable. To do this requires
replacing file system calls by the `clfs` version and skipping any reading.

Function `dirty-p` does not read file contents so it only requires replacing
lisp or uiop functions by the clfs versions. A possible implementation is:

```
(defun dirty-p (in out)
  (if (clfs:file-exists-p in)
      (or (not (clfs:file-exists-p out))
          (< (clfs:file-write-date out)
             (clfs:file-write-date in)))
      (error 'file-error :pathname in)))
```

This implementation can be called from within a sandbox because it uses 
only `clfs` functions for file access.

Functions `read-contents` and `write-converted` are the problematic
cases because they manipulate file contents. Using `clfs` function 
`execute-p` the calls to them can be skipped as follows:

```
(defun convert-file (in out)
  (when (dirty-p in out)
    (clfs:with-open-file (s out
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
      (when (clfs:execute-p)
        (write-converted (read-contents in) s)))))
```

During a simulation this version still creates the file, but the
problematic reading and writing is skipped. In this way function
`convert-file` is made suitable for calls from a sandbox.

To make a function sandboxable it is sufficient to replace file 
system calls by the `clfs` version and avoid reading (and writing)
files, but to take advantage of `clfs`'s pre- and post-conditions 
you need to wrap the sandboxable version in a contract definition.



### Define Your Own Contracts

You can define your own contracts with macro `defaction`. This macro
expects a name and arguments and defines a function just like macro
`defun`, but instead of a body it expects four bodies, one for the
pre-condition, one for the body, one for the post-condition and one
for the difference condition. A difference condition is a special post-
condition that compares before and after snapshots of the file system.


In this section we will define action `convert-file` to convert a file,
but to keep it simple the call to `dirty-p` is omitted. The dirty check 
makes the conditions more complex and is handled in the next section. 
In this section the call is kept outside the function. A possible use is 
as follows.

```
(with-sandbox (dir)
  (when (dirty-p in out)
    (convert-file in out)))
```

A possible action definition without the dirty check is:

```
(clfs:defaction convert-file (in out)
  "Reads the contents of file in, converts it and writes the result in
  file out."

  (:pre-condition
   (and (clfs:file-exists-p in)
        (not (clfs:file-exists-p out))
        (not (clfs:directory-exists-p out))
        (clfs:directory-exists-p
         (uiop:pathname-directory-pathname out))))

  (:body
   (clfs:with-open-file (s out
                               :direction :output
                               :if-does-not-exist :create)
     (when (clfs:execute-p)
       (write-converted (read-contents in) s)))
   (values))

  (:post-condition
   (declare (ignore in))
   (lambda () (clfs:file-exists-p out)))

  (:difference
   (declare (ignore in))
    (lambda ()
      (lambda (removed added)
        (and (null removed)
             (equal added (list (clfs:truename out))))))))
```

The code defines action `convert-file` that expects arguments `in` and
`out`.


The pre-condition is a logical expression that is called before the
action body. In this case it tests that the output file does not
already exist and that the output file's directory does exist.


The action body is the actual function body. Inside the body function
`execute-p` and `simulate-p` can be used to test if the action is
executed or simulated, just as in the previous section.


The post-condition is a function that expects the action body's
result. It is called before the action's body, and the outcome is
applied to the action body's result. The function is a logical
expression that tests the success of the action. In this case the
action returns no value so the function's argument list is empty. The
condition tests that the file exists.


In a difference condition the logical expression is replaced by a
function of two arguments, one for the files that were removed during
the action, and one for the files that were added. The function is a
logical expression that test whether this difference in the file
system is correct. In this case it tests that no files are removed and
that only the created file is added.


With the contract defined, function `convert-file` can now be called
from inside a sandbox and can also be tested automatically.



### Another Contract Example

The action from the previous section was relatively simple, but you
often encounter situations were the conditions need to remember some
state. An example is when the dirty test is part of the convert function.


In this section we use function `dirty-p` inside function
`convert-file` and return a non-nil value if the file was written and
nil otherwise. A possible definition is as follows.

```
(clfs:defaction convert-file (in out)
  "Reads the contents of file in, converts it and writes the result in
  file out."

  (:pre-condition
   (and (clfs:file-exists-p in)
        (clfs:directory-exists-p
         (uiop:pathname-directory-pathname out))))

  (:body
   (handler-case
       (when (dirty-p in out)
         (clfs:with-open-file (s out
                                 :direction :output
                                 :if-does-not-exist :create
                                 :if-exists :supersede)
           (when (clfs:execute-p)
             (write-converted (read-contents in) s)))
         t)
     (file-error () nil)))

  (:post-condition
   (let ((needed-update (dirty-p in out)))
     (lambda (result)
       (and (implies result needed-update)
            (implies result (clfs:file-exists-p out))))))

  (:difference
   (declare (ignore in))
    (let ((existed (clfs:file-exists-p out)))
      (lambda (result)
        (lambda (removed added)
          (and (null removed)
               (if (and result (not existed))
                   (equal added (list (clfs:truename out)))
                   (null added))))))))
```

This versions differs at various points with the previous version. The
pre-condition no longer requires that the file does not exist. The
body only create the file when necessary and returns a non-nil value
if it did. The post-condition now gets a result. To test it, it needs
to remember if the file was dirty or not. The difference condition now
needs to remember whether the file existed or not.



### List of Shadowed Functions

Common Lisp actions 

* `close`
* `compile-file`
* `delete-file`
* `ensure-directories-exist`
* `open`
* `rename-file`


Common Lisp observers

* `directory`
* `file-author`
* `file-length`
* `file-position`
* `file-write-date`
* `probe-file`
* `truename`


Uiop actions

* `chdir`
* `combine-fasls`
* `compile-file*`
* `concatenate-files`
* `copy-file`
* `delete-directory-tree`
* `delete-empty-directory`
* `delete-file-if-exists`
* `ensure-all-directories-exist`
* `ensure-pathname`
* `rename-file-overwriting-target`
* `save-deferred-warnings`


Uiop observers

* `collect-sub*directories`
* `directory*`
* `directory-exists-p`
* `directory-files`
* `file-exists-p`
* `getcwd`
* `parse-file-location-info`
* `parse-windows-shortcut`
* `probe-file*`
* `resolve-symlinks`
* `resolve-symlinks*`
* `safe-file-write-date`
* `subdirectories`
* `truename*`
* `truenamize`



### Random Testing

The package contains an experimental test driver for automated random testing. 
Call function `(clfs-test:test-contracts " some directory here ")` to test all 
file system actions. See directory test/ in the source code.



### Status

The implemented shadowed functions just test access and redirect to the
underlying implementation. This should work without any problems. A few
functions are still missing.

The contracts is work in progress. Requires more testing with different
implementations on different file systems.

The test case generator works, but is still basic.

The virtual file system misses some features.

Some stream write functions have been implemented, but this is not
worked on at the moment.



### Links

Relevant HyperSpec links:

* [20.2 The Files Dictionary](http://www.lispworks.com/documentation/HyperSpec/Body/c_files.htm)
* [21.2 The Streams Dictionary](http://www.lispworks.com/documentation/HyperSpec/Body/c_stream.htm)
* [24.2 The System Construction Dictionary](http://www.lispworks.com/documentation/HyperSpec/Body/c_system.htm)


Relevant uiop links:

* [6 UIOP/OS](https://common-lisp.net/project/asdf/uiop.html#UIOP_002fOS)
* [7 UIOP/Pathname](https://common-lisp.net/project/asdf/uiop.html#UIOP_002fPATHNAME)
* [8 UIOP/Filesystem](https://common-lisp.net/project/asdf/uiop.html#UIOP_002fFILESYSTEM)
* [9 UIOP/Stream](https://common-lisp.net/project/asdf/uiop.html#UIOP_002fSTREAM)
* [11 UIOP/Lisp-build](https://common-lisp.net/project/asdf/uiop.html#UIOP_002fLISP_002dBUILD)

Paul Griffioen, 2020
pgriffel@gmail
