function J = computeCost(X, y, theta)
%COMPUTECOST Compute cost for linear regression
%   J = COMPUTECOST(X, y, theta) computes the cost of using theta as the
%   parameter for linear regression to fit the data points in X and y

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 
J = 0;

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost of a particular choice of theta
%               You should set J to the cost.
%X
%size(X)
%size(y)
%size(y_model)
y_model = theta(1) + theta(2)*X(:,2);
delta_y = (y-y_model).^2;
cost = (1./(2.0*m))*sum(delta_y);

J = cost;



% =========================================================================

end
