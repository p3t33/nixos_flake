This files are the instructions on decryption and the use of secret files(path,
ownership...), they will not work without the actual encrypted files inside of
machine/<name>/secrets directory.

The configurations in this directory can be considered as complimentary to
the main configurations, meaning they produce secrets to be used by other
tools(E.g the credentials for git).

The reason they are separated from the main configurations, is code reuse, as some
machines might need the tool but not the secrets.

