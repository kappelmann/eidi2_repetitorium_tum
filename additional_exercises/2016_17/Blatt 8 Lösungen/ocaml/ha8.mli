val get_in_edges :
  Ha8_angabe.cfg -> Ha8_angabe.node_id -> Ha8_angabe.edge list
val get_out_edges :
  Ha8_angabe.cfg -> Ha8_angabe.node_id -> Ha8_angabe.edge list
val must_compute_wp : Ha8_angabe.cfg -> Ha8_angabe.node_id -> bool
val can_compute_wp : Ha8_angabe.cfg -> Ha8_angabe.node_id -> bool
val set_wp :
  Ha8_angabe.cfg ->
  Ha8_angabe.node_id -> Ha8_angabe.formula -> Ha8_angabe.cfg
val normalize_boolean_expression :
  Ha8_angabe.boolean_expression -> Ha8_angabe.boolean_expression
val negate_boolean_expression :
  Ha8_angabe.boolean_expression -> Ha8_angabe.boolean_expression
val simplify_boolean_expression :
  Ha8_angabe.boolean_expression -> Ha8_angabe.boolean_expression
val simplify_implication : Ha8_angabe.implication -> Ha8_angabe.implication
val is_tautology : Ha8_angabe.implication -> bool
val simplify_formula : Ha8_angabe.formula -> Ha8_angabe.formula
val replace_var_in_linear_expression :
  Ha8_angabe.linear_expression ->
  Ha8_angabe.var ->
  Ha8_angabe.linear_expression -> Ha8_angabe.linear_expression
val replace_var_in_boolean_expression :
  Ha8_angabe.boolean_expression ->
  Ha8_angabe.var ->
  Ha8_angabe.linear_expression -> Ha8_angabe.boolean_expression
val compute_wp : Ha8_angabe.cfg -> Ha8_angabe.node_id -> Ha8_angabe.formula
val verify : Ha8_angabe.cfg -> Ha8_angabe.cfg
