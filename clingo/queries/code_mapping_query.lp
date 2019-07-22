% code_mapping_query.lp

% To run from the clingo toplevel ...
% > clingo facts/code_mapping.lp queries/code_mapping_query.lp


map_to_aib(X,Y,Z) :- code_mapping(X,Y,Z, "WTN","WTF","AST").

#show map_to_aib/3.
