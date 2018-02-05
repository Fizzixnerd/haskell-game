#version 450 core

out VS_OUT {
  vec4 color;
} vs_out;

void main() {
  const vec4 vertices[] = vec4[](vec4(0.25, -0.25, 0.5, 1.0),
		                 vec4(-0.25, -0.25, 0.5, 1.0),
		                 vec4(0.25, 0.25, 0.5, 1.0));
				   
  gl_Position = vertices[gl_VertexID]; //+ offset;
  vs_out.color = vec4(0.8, 1.0, 0.0, 1.0);
}
