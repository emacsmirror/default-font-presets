####################
Default Font Presets
####################

This is a simple package for switching between a user defined list of fonts,
with support for scaling the default font too.

Available via `melpa <https://melpa.org/#/default-font-presets>`__.


Motivation
==========

While setting the default font isn't difficult,
there are times when it may be desirable to change the current font for a running Emacs instance.

In this case, users may have a smaller set of preferred fonts to choose from,
so selecting the font and it's size from a global list of all fonts isn't very convenient.


Usage
=====

This package doesn't require any code to run on startup or define a minor-mode, key maps etc.

There is no need to change how you set your default font either.

Instead, simply map keys to actions you wish to use.


``default-font-presets-forward``
   Cycle to the next available font.
``default-font-presets-backward``
   Cycle to the previous available font.
``default-font-presets-scale-increase``
   Increase the current font size by one point.
``default-font-presets-scale-decrease``
   Decrease the current font size by one point.
``default-font-presets-scale-reset``
   Reset the current font size.

Example configuration, using use-package (with ``straight``):

.. code-block:: elisp

   (use-package default-font-presets

     :commands
     (default-font-presets-forward
      default-font-presets-backward
      default-font-presets-choose
      default-font-presets-scale-increase
      default-font-presets-scale-decrease
      default-font-presets-scale-reset)

     :config
     (setq default-font-presets-list
       (list
         "Cascadia Code-13:spacing=90"
         "Fantasque Sans Mono Medium 14"
         "IBM 3270 Medium 17"
         "Input Mono Light 15"
         "JetBrains Mono-13:spacing=100"
         "Monoid Medium 13"
         "Source Code Pro Medium 10"
         "agave-14.9"))))

   ;; Typical key bindings:

   (global-set-key (kbd "C-=") 'default-font-presets-scale-increase)
   (global-set-key (kbd "C--") 'default-font-presets-scale-decrease)
   (global-set-key (kbd "C-0") 'default-font-presets-scale-reset)

   (global-set-key (kbd "<C-mouse-4>") 'default-font-presets-scale-increase)
   (global-set-key (kbd "<C-mouse-5>") 'default-font-presets-scale-decrease)

   ;; Alt-PageUp, Alt-PageDown.
   (define-key global-map (kbd "<M-prior>") 'default-font-presets-forward)
   (define-key global-map (kbd "<M-next>") 'default-font-presets-backward)


Customization
-------------

``default-font-presets-list`` (``nil`` a list of strings)
   Font presets to use.
``default-font-presets-reset-scale-on-switch`` (``t``)
   When not ``nil`` switching fonts resets any scaling.


Details
=======


- Changes are applied to all windows globally.
- The current font is always added to the list, if it's not already present.
- If the font can't be found it will be skipped when cycling forward/backwards
  *(with a note that fonts were skipped)*.
- Scaling the font simply adjusts it by one point size.
- While scaled font sized are rounded, the un-scaled font size can have a fractional size.
