# rally-mode.el - A Rally mode for Emacs


## Installation ##

Install from MELPA via package.el.


Sample config:

    
	  (require 'rally-mode)
      (setq rally-user "my-rally-user@example.com")


## Use ##

      M-x rally-current-iteration

Then, enter username and password when prompted, and it should display current sprint task info for your user.


## Actions ##

* Refresh (rally-current-iteration) - bound to g by default.
* Show Description (rally-get-description) - bound to space key by default.
* Redraw (rally-draw-results) - bound to r key by default.


## TODO ##
* Add a way to close a task.
* Add a way to move forward/back through sprints.
* Add a way to jump right to work item (story/defect) via browser.
* Add a way to show others' tasks in same sprint/work item.
