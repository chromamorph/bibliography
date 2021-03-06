function tf = lexLessOrEqual(u, v, k)

% Copyright 2012 Tom Collins

% This function returns 1 if u is 'less than' v, where 'less than' is the
% lexicographic ordering. It returns 'equal' if u = v, and 0 otherwise. In
% general, for two vectors u and v, this function finds the first index i
% such that u(i) is not equal to v(i). If u(i) is less than v(i), then u is
% 'less than' v. If v(i) is less than u(i), then v is 'less than' u. In the
% event that u equals v, u is not 'less than' v.

% INPUT
%  u and v are k-dimensional vectors.
%  k is the dimension.

% EXAMPLE
% For example, with u and v as defined below, u is 'less than' v. But if u
% had 2 instead of 1 as its second argument, then v would be 'less than' u.
% u = [2 1 7];
% v = [2 2 4];
% k = 3;
% tf = lexLessOrEqual(u, v, k);

% Logical outcome.
tf = 'equal';
% Dimension of vector.
i = 1; % Increment over u, v.
while i <= k
    if abs(u(i) - v(i)) < 1e-10 % Test equality with tolerance for error.
        i=i+1;
    elseif u(i) < v(i)
        tf = 1;
        i = k + 1;
    else
        tf = 0;
        i = k + 1;
    end
end