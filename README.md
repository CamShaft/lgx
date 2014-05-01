expr [![Build Status](https://travis-ci.org/camshaft/expr.png?branch=master)](https://travis-ci.org/camshaft/expr)
====

Simple parallel expression engine for erlang.

Usage
-----

### Forms

Every form has a `type` field. It also should set `line` to get helpful errors that map to the correct line of the original source. `value` and `children` can be present depending on the type.

[calls](#call), [assignments](#assign) and [variables](#variable) may also have the following fields:

* `silent` - return undefined if the expression fails
* `timeout` - timeout after ms
* `spawn` - spawn the function in a wrapper process for an async response

### Runtime

The runtime behaves lazily, meaning it only evaluates the expressions when it needs the answer. It also uses a kind of error monad to handle exceptions.

#### steps

##### before

* push the root `expr` onto `pending`

##### loop

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

###### when an error occurs

* each error in errors
  * is silent?
    * set `fn` result to `undefined`
  * otherwise
    * crash

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

#### Nested

##### list

```erlang
#{
  type => list,
  children => #{
    0 => #{
      type => literal,
      value => first
    },
    1 => #{
      type => literal,
      value => second
    },
    2 => #{
      type => literal,
      value => third
    }
  }
}
```

##### tuple

```erlang
#{
  type => tuple,
  children => #{
    0 => #{
      type => literal,
      value => first
    },
    1 => #{
      type => literal,
      value => second
    },
    2 => #{
      type => literal,
      value => third
    }
  }
}
```

##### map

```erlang
#{
  type => map,
  children => #{
    first_key => #{
      type => literal,
      value => first
    },
    second_key => #{
      type => literal,
      value => second
    },
    third_key => #{
      type => literal,
      value => third
    }
  }
}
```

#### functions

##### call

```erlang
#{
  type => call,
  value => {module, function},
  children => #{
    0 => #{
      type => literal,
      value => first_argument
    },
    1 => #{
      type => literal,
      value => second_argument
    }
    2 => #{
      type => literal,
      value => third_argument
    }
  }
}
```

##### cond

```erlang
#{
  type => 'cond',
  children => #{
    %% should return 'true' or 'false'
    0 => #{
      type => call,
      value => {module, is_valid}
    },
    %% only called if 'true'
    1 => #{
      type => call,
      value => {module, truths}
    },
    %% only called if 'false'
    2 => #{
      type => call,
      value => {module, falsities}
    }
  }
}
```

or

```erlang
#{
  type => 'cond',
  children => #{
    %% should return 'true' or 'false'
    0 => #{
      type => call,
      value => {module, is_valid}
    },
    %% only called if 'true'
    1 => #{
      type => call,
      value => {module, truths}
    }
    %% otherwise returns 'undefined'
  }
}
```

##### comprehension

```erlang
#{
  type => comprehension,
  children => #{
    assignment => #{
      type => assign,
      value => 'User',
      children => #{
        0 => #{
          type => list,
          children => #{
            0 => #{
              type => literal,
              value => <<"1">>
            },
            1 => #{
              type => literal,
              value => <<"2">>
            },
            2 => #{
              type => literal,
              value => <<"3">>
            }
          }
        }
      }
    },
    filters => #{
      0 => #{
        type => call,
        value => {users, is_allowed},
        children => #{
          0 => #{
            type => variable,
            value => 'User'
          }
        }
      }
    },
    body => #{
      0 => #{
        type => assign,
        value => 'User2',
        children => #{
          0 => #{
            type => call,
            value => {users, fetch_user},
            children => #{
              0 => #{
                type => variable,
                value => 'User'
              }
            }
          }
        }
      },
      1 => #{
        type => call,
        value => {users, format},
        children => #{
          0 => #{
            type => variable,
            value => 'User2'
          }
        }
      }
    }
  }
}
```

#### Variables

##### assign

```erlang
#{
  type => assign,
  value => 'MyVariable',
  children => #{
    0 => #{
      type => literal,
      value => <<"my value">>
    }
  }
}
```

##### variable

```erlang
#{
  type => variable,
  value => 'MyVariable'
}
```

### Example

```erlang
[
  #{
    type => assignment,
    value => 'MyVar',
    children => #{
      0 => #{
        type => literal,
        value => <<"my-value">>
      }
    }
  },
  #{
    type => call,
    value => {mod, func},
    children => #{
      0 => #{
        type => literal,
        value => 1
      },
      1 => #{
      type => list,
        children => #{
          0 => #{
            type => literal,
            value => 3.14
          },
          1 => #{
            type => tuple,
            children => #{
              0 => #{
                type => literal,
                value => testing
              },
              1 => #{
                type => variable,
                value => 'MyVar'
              }
            }
          },
          2 => #{
            type => map,
            children => #{
              key1 => #{
                type => cond,
                children => #{
                  0 => #{
                    type => literal,
                      value => true
                  },
                  1 => #{
                    type => literal,
                    value => <<"hello!">>
                  },
                  2 => #{
                    type => literal,
                    value => <<"world">>
                  }
                }
              }
            }
          }
        }
      }
    }
  }
].
```

Tests
-----

```sh
$ make test
```
