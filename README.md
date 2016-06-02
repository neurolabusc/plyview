# Simple Polygon Viewer

##### About

This project displays PLY or OBJ format polygon meshes. It requires OpenGL version 3.3 and is compatible with the modern "Core" releases of OpenGL that remove legacy features (such as the matrix stack). This ensures that his project will work on OSX (which only provides core functionality for modern OpenGL) as well as Linux and Windows. This stripped down project is designed to be a simple introduction to modern OpenGL. For a more sophisticated but also more complicated project see my (Surf Ice)[https://github.com/neurolabusc/surf-ice] code.

![alt tag](https://raw.githubusercontent.com/neurolabusc/plyview/master/core.jpg)

##### Compiling

By default this project is set up to use OSX's Cocoa widgetset, as OSX's Carbon widgetset does not support modern OpenGL versions. If you wish to compile this in Linux or Windows you will need to open up the project with Lazarus, and choose Project/Options/Additions&Overrides and delete the "Cocoa" widgetset.

You can build this project from the command line. You can use the "ws" option to set the desired widgetset:

 - lazbuild -B --ws=cocoa plyview.lpr

 - lazbuild -B --ws=gtk2 plyview.lpr

 - lazbuild -B --ws=win32 plyview.lpr

##### Recent Versions

 - 11/2015 Initial release

 - 6/2016 Added support for Wavefront OBJ format meshes. New Decimate menu item allows one to reduce level of detail [using fast quadric mesh simplification](https://github.com/neurolabusc/Fast-Quadric-Mesh-Simplification-Pascal-).

##### License

 This software includes a [BSD license](https://opensource.org/licenses/BSD-2-Clause)

##### Links

 - [Sample PLY format images](http://people.sc.fsu.edu/~jburkardt/data/ply/ply.html)

 - [Sample OBJ format meshes](http://people.sc.fsu.edu/~jburkardt/data/obj/obj.html)

