# Xcache Specifications

Xcache is a key value store that uses flat files to store data. The initial specifications are simply following the existing implementation on Pascal's computer with possibily to diverge from it later. 

Assuming that one want to store the string "Skywalker" against the key "Luke", we first compute the sha1 digest of "Luke", given by "0e5372f100174fe9e96bca02acdc1e5a22adb9db". This implies that the location of the file will be 

```
<xcache root directory>/datablobs/0e/53/sha1-0e5372f100174fe9e96bca02acdc1e5a22adb9db
```

The contents of the file then consists in the string "Skywalker". The reason for hashing the key is to allow keys that may not be suitable as file names. The value can be any binary data, not necessarily printable characters.

To remain compatible with the original Xcache, another file needs to be written, whose location is  

```
<xcache root directory>/timestamps/0e/53/sha1-0e5372f100174fe9e96bca02acdc1e5a22adb9db
```

and whose contents consist in the current unixtime ("1449151539" at the time those lines are written). This timestamp file needs to be written both at read and write operations against the key. (This will change later.)

The `<xcache root directory>` can be specified with the environment variable `XCACHEROOT`.
