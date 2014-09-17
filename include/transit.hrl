-record(tagged_value,
	{tag :: binary(),
	 rep :: any() }).
	 
-record(write_handler, {
	tag :: fun((any()) -> binary()),
	rep :: fun((any()) -> any()),
	string_rep :: fun((any()) -> binary())}).

