
-type sample_func() :: mfa().
-record(sampler,{
	  name            :: undefined | term(),
	  scope           = local :: local | global,
	  gproc_property,
	  gproc_label,
	  mode            :: absolute | counter,
	  sample_interval :: pos_integer() | manual_sampling,
	  history_length  :: pos_integer(),
	  sample_func     :: sample_func()}
       ).
