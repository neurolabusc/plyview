# Simple Polygon Viewer

##### About

This project displays PLY or OBJ format polygon meshes. It requires OpenGL version 3.3 and is compatible with the modern "Core" releases of OpenGL that remove legacy features (such as the matrix stack). This ensures that his project will work on OSX (which only provides core functionality for modern OpenGL) as well as Linux and Windows. This stripped down project is designed to be a simple introduction to modern OpenGL. For a more sophisticated but also more complicated project see my (Surf Ice)[https://github.com/neurolabusc/surf-ice] code.

![alt tag](https://raw.githubusercontent.com/neurolabusc/plyview/master/core.jpg)

##### Compiling

For Windows and Linux users you should be able to open this project with the Lazarus IDE and compile the project using the Run/Run menu item. For OSX things are a bit more complicated because the default OSX widgetset for Lazarus is Carbon, which does not support modern OpenGL. Therefore, if you wish to compile this on Lazarus using OSX you will need to open up the project with Lazarus, and choose Project/Options/Additions&Overrides and choose "MacOS" instead of "Default" from the "Build Modes" pull down menu. After this you can compile the project using the Run/Run command. The reason for this is that be default Lazarus builds to the older "Carbon" widgetset for MacOS, whereas modern OpenGL is only available from the "Cocoa" widgetset.

You can also build this project from the command line. For Linux and Windows, the command is like this:

 - lazbuild -B plyview.lpr

Whereas on MacOS, you will use this command:

 - lazbuild -B --ws=cocoa plyview.lpr




##### Recent Versions

 - 11/2015 Initial release

 - 6/2016 Added support for Wavefront OBJ format meshes. New Decimate menu item allows one to reduce level of detail [using fast quadric mesh simplification](https://github.com/neurolabusc/Fast-Quadric-Mesh-Simplification-Pascal-).

 - 4/2017 This project can now be compiled to the GL/GLEXT libraries (uncomment the line "{$DEFINE GLEXT}", note this will NOT compile for MacOS), or dglOpenGL (uncomment the line "{$DEFINE DGL}") or glcorearb (the default build). Note the program works identically for all libraries. The advantage of using the glcorearb is that it ensures developers do not use OpenGL commands and constants that are not supported by Core OpenGL. In other words, while the dglOpenGL library will work fine on any OS, the resulting program will crash on MacOS if the user includes functions or constants not supported by the Core specification. Compiling to the glcorearb library ensures that only supported commands are used.

 - 6/2017 Modified glcorearb.pas to work with more Windows-based OpenGL drivers. Optional retina support for MacOS (shift+click to turn feature on or off).


##### License

 This software includes a [BSD license](https://opensource.org/licenses/BSD-2-Clause)

##### Links

 - [Sample PLY format images](http://people.sc.fsu.edu/~jburkardt/data/ply/ply.html)

 - [Sample OBJ format meshes](http://people.sc.fsu.edu/~jburkardt/data/obj/obj.html)

