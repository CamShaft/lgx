-record(state, {
  ref :: reference(),
  cache = #{} :: map(),
  cache_hits = 0 :: integer(),
  values = #{} :: map(),
  vars = #{} :: list(),
  pending = [] :: list(),
  counter = 0 :: integer(),
  waiting = 0 :: integer(),
  completed = 0 :: integer(),
  errors = [] :: list(),
  map :: fun(),
  context :: any(),
  iterations = 0 :: integer(),
  stalled = 0 :: integer(),
  pids = [] :: list(),
  calls = [] :: list()
}).

-record(expr, {
  id,
  type = literal,
  line,
  value,
  deps = -1,
  is_root = false,
  children = [],
  status = added,
  tmp
}).
