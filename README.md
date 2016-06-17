# blablabla [![Paypal logo](https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=TAWNECQR3TTUY)

WIP litable replacement.

We currently support loads of things the old litable wasn't able to do, including

* proper lexical vs dynamic binding
* lambda forms instrumentation
* proper setq/let instrumentation (following the sequence rules of elisp)
* macros are properly *not* instrumented (but I'm working on debug declaration parsing!)
* somehow sensible error handling (print error message instead of result after the function)
* **automatic reinstrumentation** = the "highlight" instrumentation is updated on-the-fly as you edit the function, no user input required
* we have tests now :)

![Lexical binding preview](http://i.imgur.com/GjK8gPN.png)
