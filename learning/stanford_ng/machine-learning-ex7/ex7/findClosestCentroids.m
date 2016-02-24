function idx = findClosestCentroids(X, centroids)
%FINDCLOSESTCENTROIDS computes the centroid memberships for every example
%   idx = FINDCLOSESTCENTROIDS (X, centroids) returns the closest centroids
%   in idx for a dataset X where each row is a single example. idx = m x 1 
%   vector of centroid assignments (i.e. each entry in range [1..K])
%

% Set K
K = size(centroids, 1);

% You need to return the following variables correctly.
idx = zeros(size(X,1), 1);

% ====================== FM CODE HERE ======================
% Instructions: Go over every example, find its closest centroid, and store
%               the index inside idx at the appropriate location.
%               Concretely, idx(i) should contain the index of the centroid
%               closest to example i. Hence, it should be a value in the 
%               range 1..K
%
% Note: You can use a for-loop over the examples to compute this.



dist_matrix = zeros(size(X,1),K);
dim_X = size(X,2);
dist_centroid_coord = zeros(size(X));


%calculate distances
for i=1:K   
    for j=1:dim_X
        dist_centroid_coord(:,j) = (X(:,j)-centroids(i,j)).^2;
    end
    dist_matrix(:,i) = sum(dist_centroid_coord,2);
end

%calculate the closest centroid
for i=1:length(idx)
    dist_to_cluster_ind = dist_matrix(i,:);
    idx(i) = min(find(dist_to_cluster_ind==min(dist_to_cluster_ind)));
end

%dist_matrix(1:10,:)
%centroids
%whos


% =============================================================

end

