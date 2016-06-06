# Simple Polygon Viewer

##### About

This project displays PLY or OBJ format polygon meshes. It requires OpenGL version 3.3 and is compatible with the modern "Core" releases of OpenGL that remove legacy features (such as the matrix stack). This ensures that his project will work on OSX (which only provides core functionality for modern OpenGL) as well as Linux and Windows. This stripped down project is designed to be a simple introduction to modern OpenGL. For a more sophisticated but also more complicated project see my (Surf Ice)[https://github.com/neurolabusc/surf-ice] code.

![alt tag](https://raw.githubusercontent.com/neurolabusc/plyview/master/core.jpg)

##### Compiling

For Windows and Linux users you should be able to open this project with the Lazarus IDE and compile the project using the Run/Run menu item. For OSX things are a bit more complicated because the default OSX widgetset for Lazarus is Carbon, which does not support modern OpenGL. Therefore, if you wish to compile this on Lazarus using OSX you will need to open up the project with Lazarus, and choose Project/Options/Additions&Overrides insert "Cocoa" as the target widgetset. After this you can compile the project using the Run/Run command.

You can also build this project from the command line. You can use the "ws" option to set the desired widgetset:

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

