function BW = pointset2bw(D, dims)

% Copyright 2012 Tom Collins

% Returns a bitmap image representing a two-dimensional set of integer
% points, converting each point to a one in the bitmap, and zeros
% everywhere else. The dimensions for the bitmap (number of rows and
% columns) are also required, but this could be estimated from the point
% set.

% INPUT
%  D is a two-dimensional point set (m x 2 matrix).
%  dims is a vector [nrow ncol] for creating the bitmap.

% EXAMPLE INPUT
% D = [1, 5; 2, 4; 3, 1; 3, 3; 3, 5; 4, 2; 4, 3; 4, 4];
% dims = [4 5];
% BW = pointset2bw(D, dims);
% imagesc(BW);

BW = zeros(dims);
nrow = size(D, 1);
for irow = 1:nrow
    BW(D(irow, 1), D(irow, 2)) = 1;
end

end
