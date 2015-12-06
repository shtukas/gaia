# Gaia

Gaia is a spin-off/re-implementation of [Nyx](https://github.com/shutkas/Nyx-Ruby) in Haskell where the regular file system is the repository.

# Vocabulary

There are two repositories: The regular file system tree itself, and Xcache (at least one instance of it). Gaia only reads from the regular file system tree (from root folders that are specified by the user) and stores data in Xcache.

The data stored in Xcache are called Aion points. They are defined as JSON objects. Aion points are represented inside the program as Aeson Values. We also have the notion of Gaia projection of Aeson Values by which we extract from Aeson Values the data contained in the Value in a way that is more user friendly.

# Todo
- Allow users to specify the location of their Xcache repository. (Currently hardcoded to use the existing one on Pascal's computer.)
- To be able to scan more than one location. 	
- Use the filepath package and its System.FilePath for multiplatform interfaces for file access

# Roadmap
- *In progress*: Recovering the functionalities of Gaia-Genesis 
- Implementing the web server and have basic online search
- Developping the Directives functionality
