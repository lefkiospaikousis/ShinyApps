## How to download ggplots and use shiy modules for many download buttons


This is an app I created to teach myself about shiny modules

What I wanted to do was first learn how to download a ggplot object to incorporate a `download` button in a Clustering app I am creating. This could be accomplished by using 2 pieces of information.a) A Download button that is associated with b) a Download hander fuction to handle the call to download a plot. However, I had a bunch of plots I need to desing a download process for. Enter Shiny Modules:

With Shiny modules, the idea is to write up ONE module that creates a button and an associated handler, and use this module to render a button to each plot I had.

As an added benefit of this `function` style module, I can declare the button label and file name to be used when saving (see the button labels)


What I struggled with:    

* How to pass reactive plots in the module server function.

Pass them without the `()` but then wrap the object wit `()` inside the server function

* Naming conventions are a must   

Otherwise you are lost..

