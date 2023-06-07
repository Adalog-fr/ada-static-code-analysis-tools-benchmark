function crea_sample_array(x, fun, filename, package_name, prefix)

  if nargin < 3
    filename = '';
  end

  if nargin < 4
    package_name = '';
  end

  if nargin < 5
    prefix ='';
  end

  if ~isempty(package_name) && (isempty(filename) || filename(end)=='/')
    dirname = filename;
    basename = package_name;
    idx = find(basename == '.');
    basename(idx)='-';
    
    filename = [dirname basename  '.ads'];
  end

  if iscell(x)
    x = expand(x);
  end

  y = apply_fun(fun, x);
  
  
  if isempty(filename) || strcmp(filename, '-')
    fid = 1;

  else
    fid = fopen(filename, 'w');

  end

  if ~isempty(package_name)
    fprintf(fid, 'package %s is\n', package_name);
    fprintf(fid, 'Samples : constant %sSample_Array :=\n', prefix);
  end

  fprintf(fid, '(\n');

  for col=1:size(x, 2)
    fprintf (fid, '(%s, %s)', to_s(x(:,col)), float_to_hex(y(col)));

    if col < size(x,2)
      fprintf(fid, ',\n');
    else
      fprintf('\n');
    end
  end
  
  %fprintf(fid, '(%20.16e, %20.16e),\n', [x(1:end-1) ; y(1:end-1)]);
  %fprintf(fid, '(%20.16e, %20.16e)\n', [x(end) ; y(end)]);
  fprintf(fid, ');\n');

  % if ~isempty(extra_par)
  %   fprintf(fid, '\nExtra : constant Float_Type :=%20.16e;\n', extra_par);
  % end

  if ~isempty(package_name)
    fprintf(fid, 'end %s;\n', package_name);
  end
  
  if fid != 1
    fclose(fid);
  end

end

function result = expand(data)
  result = data{1};
  result = result(:)';

  data = {data{2:end}};

  while ~isempty(data)
    u = data{1};
    u = u(:)';
    data = {data{2:end}};

    result = [kron(result, ones(1, size(u, 2)))
	      kron(ones(1, size(result,2)), u)];
		   
  end
end

function result=to_s(x)
  x = x(:);

  result = '';

  for k=1:length(x)
    result = [result float_to_hex(x(k)) ', '];
  end
  
  % Remove the spurious ', ' at the end
  result(end-1:end) =[];
  
  if size(x, 1) > 1
    result = ['(' result ')'];
  end
end

function y=apply_fun(fun, x)
  y = zeros(1, size(x, 2));

  for col=1:size(x, 2)
    u = num2cell(x(:, col));
    y(col) = fun(u{:});
  end
end
