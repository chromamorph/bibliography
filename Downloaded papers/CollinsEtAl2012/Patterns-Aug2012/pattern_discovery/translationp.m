function tf = translationp(P, Q)

% Copyright 2012 Tom Collins

% This function returns the value 1 if Q is a translation of P, and zero
% otherwise. There is no assumption of lexicographic order, but it is
% assumed that P and Q have unique rows and the same (column) dimension.

% INPUT
%  P is a point set.
%  Q is a point set.

% EXAMPLE
% P = [0 0; 0 3; 1 2];
% Q = [2 -1; 2 2; 3 1];
% tf = translationp(P, Q);

tf = 1;
m = size(P, 1);
n = size(Q, 1);
% Dimension of subsets.
k = size(P, 2);
if m ~=  n
    % If P and Q have a different number of points, Q cannot be a
    % tranlsation of P.
    tf = 0;
else
    % Put P and Q in lexicographic order.
    P = unique(P, 'rows');
    Q = unique(Q, 'rows');
    v = Q(1, :) - P(1, :);
    Ptrans = P + repmat(v, m, 1);
    i = 1;
    while i <= m
        if sum(Ptrans(i, :) == Q(i, :)) == k
            i=i+1;
        else
            tf = 0;
            i = m + 1;
        end
    end
end

end
