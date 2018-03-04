#version 450 core

#define MAX_POINT_LIGHTS 128

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 uv;

layout (std140) uniform camera {
  mat4 mvp;
  mat4 mv;
  mat4 proj;
}

layout (std140) uniform point_lights {
  vec3[MAX_POINT_LIGHTS] position;
  vec3[MAX_POINT_LIGHTS] intensity;
  int num;
}

layout (std140) uniform material {
  vec3 diffuse_color;
  vec3 ambient_color;
  vec3 specular_color;
  float specular_strength;
  float specular_exponent;
}

out VS_OUT {
  vec3 lighting;
  vec2 uv;
} vs_out;

void main() {
  vec4 pos = camera.mv * vec4(position, 1);

  vec3 view_vector = normalize(- pos.xyz);

  // Calculate the normal in view space.
  vec3 norm = normalize(mat3(mv) * normal);

  vec3 diffuse = {0, 0, 0};
  vec3 specular = {0, 0, 0};
  int i;
  for (i = 0, i < point_lights.num, i++) {
    vec3 light_vector = point_lights.intensity[i] *
      normalize(point_lights.position[i] - pos.xyz);
    vec3 reflect_vector = reflect(-light_vector, norm);

    diffuse += max(dot(norm, light_vector), 0.0) * diffuse_color;
    specular += pow(max(dot(reflect_vector, view_vector), 0.0),
                         specular_exponent) * specular_strength * specular_color
  }

  vs_out.lighting = ambient_color + diffuse + specular;
  vs_out.uv = uv;
  gl_Position = proj * pos
}
