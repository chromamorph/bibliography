function [P, I] = maximalTranslatablePattern(v, D, k, n)

% Copyright 2012 Tom Collins

% This function calculates the maximal translatable pattern (MTP) of the
% vector v in the k-dimensional point set D (Meredith, Lemstrom, & Wiggins,
% 2002). The MTP P and indices I of datapoints forming the MTP are
% returned.

% REFERENCE
%  David Meredith, Kjell Lemstrom, and Geraint A. Wiggins. Algorithms for
%   discovering repeated patterns in multidimensional representations of
%   polyphonic music. Journal of New Music Research, 31(4) (2002), 321-345.

% INPUT
%  v is a k-dimensional vector.
%  D is a k-dimensional point set. It is assumed that D is in lexicographic
%   order. Otherwise
%   D = sortrows(D);
%   can be used to achieve this. Alternatively,
%   D = unique(D, 'rows');
%   will also remove duplicate datapoints.
%  k is the dimension.
%  n is the number of points in D.

% EXAMPLE
% v = [12 2 0];
% D = [0 69 0.5; 0.5 66 0.5; 1 67 1; 2 65 1; 3 66 0.5; 3.5 64 0.5;...
%     4 62 2; 5 55 1; 5 59 1; 5 61 1; 6 63 0.5; 6.5 64 0.5; 7 55 1;...
%     7 56 1; 7 61 1; 7 65 1; 8 55 1; 8 57 1; 8 61 1; 8 66 1; 9 51 1;...
%     9 67 0.5; 9.5 70 0.5; 10 55 1; 10 60 1; 10 69 2; 11 51 1; 12 50 1;...
%     12 71 0.5; 12.5 68 0.5; 13 57 1; 13 60 1; 13 62 1; 13 69 1;...
%     14 60 1; 14 62 1; 14 65 1; 14 67 1; 15 68 0.5; 15.5 66 0.5;...
%     16 57 1; 16 61 1; 16 63 1; 16 64 2; 17 57 1; 17 59 1; 17 63 1;...
%     18 65 0.5; 18.5 66 0.5; 19 57 1; 19 58 1; 19 63 1; 19 67 1;...
%     20 57 1; 20 59 1; 20 63 1; 20 68 1];
% k = 3;
% n = 57;
% [P, I] = maximalTranslatablePattern(v, D, k, n);
% hold on
% plot(D(:,1),D(:,2),'xk');
% plot(P(:,1),P(:,2),'.b');
% plot(P(:,1)+v(1), P(:,2)+v(2),'.g');
% hold off

% E = D + repmat(v,n,1);
P = zeros(n,k);
I = zeros(n,1);
i = 1; % Increment over D.
j = 1; % Increment over E.
L = 1; % Increment over P.
while i <= n
    % Does not seem to be any time saved by using lexLessOrEqualTriple
    % instead of lexLessOrEqual, so could revert to latter.
    tf = lexLessOrEqualTriple(D(j,:), v, D(i,:), k);
    if tf
        if strcmp(tf, 'equal')
            P(L,:) = D(j,:);
            I(L) = j;
            i=i+1;
            j=j+1;
            L=L+1;
        else
            j=j+1;
        end
    else
    i=i+1;
    end
end
P = P(1:L-1,:);
I = I(1:L-1);

end
