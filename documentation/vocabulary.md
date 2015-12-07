## Vocabulary

There are two repositories: The regular file system tree itself, and Xcache (at least one instance of it). Gaia only reads from the regular file system tree (from root folders that are specified by the user) and stores data in Xcache.

The data stored in Xcache are called **Aion points**. They are defined as JSON objects. Aion points are represented inside the program as **Aeson Values**. We also have the notion of **Gaia projection** of Aeson Values by which we extract from Aeson Values the data contained in the Value in a way that is more user friendly. **FilePath** is a complete FS path to a file. **FolderPath** is a complete FS path to a directory. The term used to refer to both FilePaths and FolderPaths is **LocationPath**.
