expr [![Build Status](https://travis-ci.org/camshaft/expr.png?branch=master)](https://travis-ci.org/camshaft/expr)
====

Simple parallel expression engine for erlang.

Usage
-----

### Forms and expressions

Every form and/or has a `type` field. It also should set `line` to get helpful errors that map to the correct line of the original source. `value` and `children` can be present depending on the type.

[calls](#call), [assignments](#assign) and [variables](#variable) may also have the following fields:

* `silent` - return undefined if the expression fails
* `timeout` - timeout after ms
* `spawn` - spawn the function in a wrapper process for an async response

### Types

#### Literals

##### atom

```erlang
#{
  type => literal,
  value => atom
}
```

##### binary

```erlang
#{
  type => literal,
  value => <<"binary">>
}
```

##### float

```erlang
#{
  type => literal,
  value => 3.14
}
```

##### integer

```erlang
#{
  type => literal,
  value => 123
}
```

#### Compound

##### list

```erlang
#{
  type => list,
  children => [
    #{
      type => literal,
      value => first
    },
    #{
      type => literal,
      value => second
    },
    #{
      type => literal,
      value => third
    }
  ]
}
```

##### tuple

```erlang
#{
  type => tuple,
  children => [
    #{
      type => literal,
      value => first
    },
    #{
      type => literal,
      value => second
    },
    #{
      type => literal,
      value => third
    }
  ]
}
```

##### map

```erlang
#{
  type => map,
  children => [
    #{
      type => tuple,
      children => [
        #{
          type => literal,
          value => first_key
        },
        #{
          type => literal,
          value => first_value
        }
      ]
    },
    #{
      type => tuple,
      children => [
        #{
          type => literal,
          value => second_key
        },
        #{
          type => literal,
          value => second_value
        }
      ]
    },
    #{
      type => tuple,
      children => [
        #{
          type => literal,
          value => third_key
        },
        #{
          type => literal,
          value => third_value
        }
      ]
    }
  ]
}
```

#### functions

##### call

```erlang
#{
  type => call,
  value => {module, function},
  children => [
    #{
      type => literal,
      value => first_argument
    },
    #{
      type => literal,
      value => second_argument
    }
    #{
      type => literal,
      value => third_argument
    }
  ]
}
```

##### cond

```erlang
#{
  type => 'cond',
  children => [
    %% should return 'true' or 'false'
    #{
      type => call,
      value => {module, is_valid}
    },
    %% only called if 'true'
    #{
      type => call,
      value => {module, truths}
    },
    %% only called if 'false'
    #{
      type => call,
      value => {module, falsities}
    }
  ]
}
```

or

```erlang
#{
  type => 'cond',
  children => [
    %% should return 'true' or 'false'
    #{
      type => call,
      value => {module, is_valid}
    },
    %% only called if 'true'
    #{
      type => call,
      value => {module, truths}
    }
    %% otherwise returns 'undefined'
  ]
}
```

##### comprehension

```erlang
#{
  type => comprehension,
  children => [
    #{
      type => assign,
      value => 'User',
      children => [
        #{
          type => list,
          children => [
            #{
              type => literal,
              value => <<"1">>
            },
            #{
              type => literal,
              value => <<"2">>
            },
            #{
              type => literal,
              value => <<"3">>
            },
            #{
              type => literal,
              value => <<"4">>
            },
            #{
              type => literal,
              value => <<"5">>
            }
          ]
        }
      ]
    },
    #{
      type => call,
      value => {maps, get},
      children => [
        #{
          type => literal,
          value => name
        },
        #{
          type => variable,
          value => 'User'
        }
      ]
    }
  ]
}.
```

#### Variables

##### assign

```erlang
#{
  type => assign,
  value => 'MyVariable',
  children => [
    #{
      type => literal,
      value => <<"my value">>
    }
  ]
}
```

##### variable

```erlang
#{
  type => variable,
  value => 'MyVariable'
}
```

### Runtime

The runtime behaves lazily, meaning it only evaluates the expressions when it needs the answer. It also uses a kind of error monad to handle exceptions.

#### loop

* each expr in pending
  * is it root and is it done?
    * yes
      * return the value from `values`
    * no
      * are the dependencies ready?
        * yes
          * push the `expr` on `fns`
        * no
          * push the dependencies on `pending`

* while messages
  * recieve with timeout 0
    * timeout
      * exit loop
    * value
      * set the return value in `values` from `pid` `id`
      * remove `pid` from `pids`
    * error
      * push `error` on `errors`

* each fn in fns
  * execute function with arguments
    * error
      * push `error` on `errors`
    * pid
      * push `pid` on `pids` with a start time
    * success
      * set the return value in `values` from `fn` `id`

#### when an error occurs

* each error in errors
  * is silent?
    * set `fn` result to `undefined`
  * otherwise
    * crash

Tests
-----

```sh
$ make test
```
