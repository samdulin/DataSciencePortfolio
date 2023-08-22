b = 1;
a = 0;
n = 7; %number of steps
N = 10; %number of shots

xspan = linspace(a, b, n)';

disp(xspan);
dx = (xspan(2) - xspan(1));

tspan = [0 1];

%Set params
tol = 1e-8;
newtonTol = 1e-8;
options = odeset('AbsTol',tol,'RelTol',tol);


%initial functions

%u = @(x) normpdf(x, 10, 1);
%v = @(x) normpdf(x, 10, 1);

%u = @(x) normpdf(x, 0, 1);
%v = @(x) normpdf(x, .3, .2);

u = @(x) normpdf(x, 0, 1);
v = @(x) normpdf(x, .3, .2);

%u = @(x) x.*(1-x).^2;
%v = @(x) x.^3;

%u = @(x) 1/6 + 0*x;
%v = @(x) normpdf(x, 0, 1);

%inital condition discretized
y0 = u(xspan);

rrrr = sum(y0)*dx;

%final condition vector
yf = v(xspan);

sum0 = sum(y0)*dx;
sumf = sum(yf)*dx;

y0 = y0/sum0;
yf = yf/sumf;


initsum = sum(y0)*dx;
finsum = sum(yf)*dx;
%init = ones(n,1)*.5;
s0 = [];
for i = 1:N
    init = (1-((i-1)/N))*y0 + ((i-1)/(N))*(yf);
    s0 = [s0; init(1:n-1); zeros(n-1,1)];
    disp(s0);
end


r = @(q0, qf) [q0(1:n-1) - y0(1:n-1); qf(1:n-1) - yf(1:n-1)];

f = @(t,y) ham(y, dx, n);

[t, y] =  multiple_shot_bvp(f, r, tspan, N, s0, newtonTol, options);

%Use Newton to find values for s values
%s = newton_div(w, s0, sqrt(eps), 20, newtonTol);

rhorho = y(:,1:n)';

mesh(rhorho);


function G = ham(q, dx, n)

p = q(1:n-1);
v = q(n:2*n-2);


w = (1 - dx.*sum(p)) ./ dx;
for i = 1:n-1
    if i == 1
        dH(i) = (v(i).*(p(i) + p(i+1))) ./ (2*(dx^2));
        dH(i+n) = -(v(i)^2)./(4.*(dx^2));
        
    elseif i == n-1
        dH(i) = (v(i-1).*(p(i) + p(i-1)) - v(i).*(p(i) + w)).*(1./(2*(dx^2)));
        dH(i+n-1) = (1./(4*dx.^2)) .* (v(i-1).^2 - v(i).^2);
        
        %dH(i) = (v(i).*(p(i) + w) - v(i-1).*(p(i) + p(i-1))).*(1./(2*(dx^2)));
        %dH(i+n) = (1./(4*dx.^2)) .* (v(i-1).^2 - v(i).^2);
        
    else
        dH(i) = (v(i-1).*(p(i) + p(i-1)) - v(i).*(p(i) + p(i+1))).*(1./(2*(dx^2)));
        dH(i+n-1) = (1./(4*dx.^2)) .* (v(i-1).^2 - v(i).^2);
        
        %dH(i) = (v(i).*(p(i) + p(i+1)) - v(i-1).*(p(i) + p(i-1))).*(1./(2*(dx^2)));
        %dH(i+n) = (1./(4*dx^2)) .* (v(i-1).^2 - v(i).^2);
        
    end

end
G = dH';
end

