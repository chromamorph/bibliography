function [D, dims] = bw2pointset(BW)

% Copyright 2012 Tom Collins

% Returns a two-dimensional set of points representing a bitmap image, by
% converting each one in the bitmap to a point, and ignoring each zero. The
% dimensions of the bitmap (number of rows and columns) are also returned,
% because this cannot be determined exactly from the point set.

% INPUT
%  BW is a bitmap image, expressed as a matrix of zeros and ones.

% EXAMPLE
% s = RandStream('mt19937ar','Seed',2);
% RandStream.setDefaultStream(s);
% BW = rand(4,5) > 0.5;
% imagesc(BW);
% [D, dims] = bw2pointset(BW);

[nrow ncol] = size(BW);
D = zeros(nrow*ncol, 2);
Di = 1;
for irow = 1:nrow
    for icol = 1:ncol
        if BW(irow, icol)
            D(Di, :) = [irow icol];
            Di = Di + 1;
        end
    end
end
D = D(1:Di-1,:);
dims = [nrow ncol];

end
