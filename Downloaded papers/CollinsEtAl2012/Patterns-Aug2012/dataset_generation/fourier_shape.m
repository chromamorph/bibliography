function [x y] = fourier_shape(npts, radius, freq, amp, phi)

% Copyright 2012 Tom Collins

% Returns two vectors x and y that describe a two-dimensional shape. To
% begin with the shape is a circle with given radius. This radius is then
% perturbed as a function of polar angle by sinusoidal compoents with given
% frequency, amplitude, and phase.

% REFERENCES
%  Weiwei Zhang and Steven J. Luck. Discrete fixed-resolution
%   representations in visual working memory. Nature 453(7192) (2008),
%   233-5.
%  Charles T. Zahn and Ralph Z. Roskies. Fourier descriptors for plane
%   closed curves. IEEE Transactions on Computers C-21(3) (1972), 269-281.

% INPUT
%  npts is a scalar specifying the number of points to be sampled from the
%   perimeter
%  radius is a scalar for the radius of the initial circle.
%  freq is a vector of frequency components in cycles per perimeter.
%  amp is a vector of amplitude components.
%  phi is a vector of phase components.

% EXAMPLE
% npts = 50;
% radius = 2;
% freq = [2 4];
% amp = [0.5 0.5];
% phi = [0 0];
% [x y] = fourier_shape(npts, radius, freq, amp, phi);

theta = 0:2*pi/npts:2*pi*(npts-1)/npts;
r = repmat(radius, 1, npts);
nharm = size(freq, 2);
for iharm = 1:nharm
    r = r + amp(iharm)*sin(freq(iharm)*theta - phi(iharm));
end
[x y] = pol2cart(theta, r);
% polar(theta, r, '*b');
% plot(theta, r, '*b');

end
