

# RRets - Round Robin Erlang Term Storage #

Copyright (c) 2014 MM Zeeman, All Rights Reserved.

### Introduction ###

The RRets packages allows for an easy way to store erlang terms into a round robin database. The 
terms are stored until the storage has reached a predefined maximum and are then overwritten by newer items. 

Each term added to the storage gets a second resolution unix_time timestamp attached to it. 
The storage can be used to easily, and quickly, store terms. Example usage:

* Logs
* System statistics
* Audit logs

The storage can be queried with a match specification which allows quick, and streaming, access 
to your data.

For example:

```erlang

MatchSpec = ets:fun2ms(fun({Timestamp, {exampe, 2, X}}) -> {Timestamp, {example, 2, X}} end),
Continuation = rrets_reader:open("my_storage", MatchSpec),
{Records, Continuation1} = rrets_reader:match(Continuation),
...
```

After opening a reader a continuation is returned. This continuation can be used to repeatedly
receive lists with matching records. In this case, all tuples matching `{example, 2, _}` terms 
are retrieved from the storage. Each matching record is returned with the timestamp when the term
was stored.


### <a name="opening">Opening a rrets storage</a> ###

RRets storages can be opened by using a name, filename and a specification on how large the
storage files need to become. The name can later be used add terms to the storage.

```erlang

Storage = rrets:open([{name, my_storage}, {file, "my_storage"}, {size, {10475520, 10}}]).
```

### <a name="log">Storing a term</a> ###

```erlang

Ts = rrets:log(my_storage, {nasi, 3})
````

### <a name="close">Closing the storage</a> ###

It is important that the storage is properly closed.

```erlang

rrets:close(my_storage)
````

