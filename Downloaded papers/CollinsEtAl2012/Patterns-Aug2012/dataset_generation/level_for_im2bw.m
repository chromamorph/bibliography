function level = level_for_im2bw(A, nstep)

% Copyright 2012 Tom Collins

% This function returns a value in [0 1] for the level parameter used to
% convert an image to black-white bitmap via the function im2bw. The level
% is chosen by interval bisection, such that the number of ones to zeros is
% close to equal.

% INPUT
%  A is an image.
%  nstep is the number of times that interval bisection will be applied.

% EXAMPLE
% s = RandStream('mt19937ar','Seed',1);
% RandStream.setDefaultStream(s);
% A = rand(40,50);
% A(10:30, 10:40) = 0.25*rand(21, 31) + 0.75;
% imagesc(A);
% level = level_for_im2bw(A, 10);

dims = size(A);
lower = 0.001;
upper = 0.999;
istep = 1;
while istep <= nstep
    % Calculate bitmaps using lower, middle, and upper values for the level
    % parameter.
    middl = (lower + upper)/2;
    im_lower = im2bw(A, lower);
    im_upper = im2bw(A, upper);
    im_middl = im2bw(A, middl);
    ones_ratio_lower = sum(sum(im_lower))/(dims(1)*dims(2));
    ones_ratio_upper = sum(sum(im_upper))/(dims(1)*dims(2));
    ones_ratio_middl = sum(sum(im_middl))/(dims(1)*dims(2));
    % Assign new value to whichever level interval produces the ratio
    % closest to 0.5, and if they tie assign new value to upper level.
    if ones_ratio_lower - ones_ratio_middl <...
            ones_ratio_middl - ones_ratio_upper
        lower = (lower + upper)/2;
    else
        upper = (lower + upper)/2;
    end
    istep = istep + 1;
end
level = middl;

end
