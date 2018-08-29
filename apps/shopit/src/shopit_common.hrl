
%% DB RECORDS
-record(store, {
          uuid = common:binary_uuid() :: binary(),
          created_at = common:get_timestamp() :: integer(), %% datetime created
          name :: binary(),
          x_coord :: float(),
          y_coord :: float()
}).

-json({store
      , {binary, "uuid"}
      , {number, "created_at"}
      , {binary, "name"}
      , {number, "x_coord"}
      , {number, "y_coord"}}).

-record(item,{
           uuid = common:binary_uuid() :: binary(),
           created_at = common:get_timestamp() :: integer(), %% datetime created
           language :: binary(),
           name :: binary()
          }).

-json({item
      , {binary, "uuid"}
      , {number, "created_at"}
      , {binary, "language"}
      , {binary, "name"}}).

-record(shopping_list_item, {
          uuid = common:binary_uuid() :: binary(),
          added_at = common:get_timestamp() :: integer(), %% datetime created
          item :: binary(), %% this will be the name of the item
          quantity :: integer(),
          quantity_left  :: integer(),
          comment :: binary(),
          added_by :: binary()
}).

-json({shopping_list_item
      , {binary, "uuid"}
      , {number, "added_at"}
      , {binary, "item"}
      , {number, "quantity"}
      , {number, "quantity_left"}
      , {binary, "comment"}
      , {binary, "added_by"}}).

-record(shopping_list, {
          uuid = common:binary_uuid() :: binary(),
          uuid_completed_part = common:binary_uuid() :: binary(),
          created_at = common:get_timestamp() :: integer(),
          expires_at = common:get_timestamp() :: integer(),
          name :: binary(),
          items :: [#shopping_list_item{}]
}).

-json({ shopping_list
      , {binary, "uuid"}
      , {binary, "uuid_completed_part"}
      , {number, "created_at"}
      , {number, "expires_at"}
      , {binary, "name"}
      , {list, "items", [{type, shopping_list_item}]}
      }).

-record(shopping_list_old, {
          uuid = common:binary_uuid() :: binary(),
          uuid_org_list :: binary(),
          bought_at_store = common:binary_uuid() :: binary(),
          created_at = common:get_timestamp() :: integer(),
          expires_at = common:get_timestamp() :: integer(),
          name :: binary(),
          items :: [#shopping_list_item{}]
}).

%% for now supersimple usertable

-record(uuid, {
          uuid = common:binary_uuid() :: binary()
}).

-json({uuid, {binary, "uuid"}}).

-record(user, {
          uuid = common:binary_uuid() :: binary(),
          name :: binary(),
          shopping_lists :: [#uuid{}]
}).

-json({ user
      , {binary, "uuid"}
      , {binary, "name"}
      , {list, "shopping_lists", [{type, binary}]}
      }).
