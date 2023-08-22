function p = newton_div(F, p0, h, maxiter, tol)

maxIterReached = true;
d = length(p0);
I = eye(d);
J = nan(d, d);
for n = 1:maxiter
    %Divided difference
    
    F0 = F(p0);
    %fprintf("|| F(p_{n}) ||      = %12.10s\n", norm(F0));
    for k = 1:d
        %disp("This is w(s0)")
        %b = (F(p0 + h * I(:,k)) - F0) ./ h;
        %disp(length(b));
        J(:,k) = (F(p0 + h * I(:,k)) - F0) ./ h;
    end
    %disp(F0)
    %disp(J);
    %Do Newton
    p = p0 - (J \ F0);
    %Prints intermediate values
    fprintf("|| F(p_{n}) ||      = %12.10s\n", norm(F0));
    fprintf("-------------------------------------\n");
    fprintf("Iteration n = %2d", n)
    display(p)
    %fprintf("%12.10f\n", p)
    fprintf("||p_{n} - p_{n-1}|| = %12.10f\n", norm(p-p0))
    %Checks if below desired tolerance
    if norm(p-p0) < tol
    %if norm(F(p)) < tol
        maxIterReached = false; 
        break; 
    end
    p0 = p;
end
fprintf("|| F(p_{n}) ||      = %12.10f\n", norm(F(p)));
fprintf("-------------------------------------\n");
%%%%%%%%%%%%%%%%%
if maxIterReached
    fprintf("Error, not converging\n");
    p = NaN;
end