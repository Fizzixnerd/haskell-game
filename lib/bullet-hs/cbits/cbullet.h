#ifndef CBULLET_H
#define CBULLET_H

#ifdef __cplusplus
extern "C" {
#endif

  // poor man's newtype.  Dear God.
  typedef struct broadphase_interface { char unused; } broadphase_interface;
  typedef struct default_collision_configuration { char unused; } default_collision_configuration;
  typedef struct collision_dispatcher { char unused; } collision_dispatcher;
  typedef struct sequential_impulse_constraint_solver { char unused; } sequential_impulse_constraint_solver;
  typedef struct discrete_dynamics_world { char unused; } discrete_dynamics_world;
  typedef struct motion_state { char unused; } motion_state;
  typedef struct static_plane_shape { char unused; } static_plane_shape;
  typedef struct sphere_shape { char unused; } sphere_shape;
  typedef struct collision_shape { char unused; } collision_shape;
  typedef struct rigid_body { char unused; } rigid_body;
  typedef struct transform { char unused; } transform;
  typedef float scalar;

  broadphase_interface* new_broadphase_interface();
  void free_broadphase_interface(broadphase_interface* broadphase);

  default_collision_configuration* new_default_collision_configuration();
  void free_default_collision_configuration(default_collision_configuration* collision_configuration);

  collision_dispatcher* new_collision_dispatcher(default_collision_configuration* collision_configuration);
  void free_collision_dispatcher(collision_dispatcher* collision_dispatcher);

  sequential_impulse_constraint_solver* new_sequential_impulse_constraint_solver();
  void free_sequential_impulse_constraint_solver(sequential_impulse_constraint_solver* constraint_solver);

  // btDiscreteDynamicsWorld
  discrete_dynamics_world* new_discrete_dynamics_world
  (collision_dispatcher* dispatcher, 
   broadphase_interface* broadphase, 
   sequential_impulse_constraint_solver* solver, 
   default_collision_configuration* collision_configuration);

  void free_discrete_dynamics_world(discrete_dynamics_world* world);

  void set_gravity(discrete_dynamics_world* world, scalar x, scalar y, scalar z);
  int step_simulation(discrete_dynamics_world* world,
		      scalar time_step,
		      const int* max_sub_steps,
		      const scalar* fixed_time_step);
  //  int default_step_simulation(discrete_dynamics_world* world, scalar time_step);
  void add_rigid_body(discrete_dynamics_world* world, rigid_body* rigid_body);

  // btDefaultMotionState
  motion_state* new_default_motion_state(scalar r, // quaternion
					 scalar i,
					 scalar j,
					 scalar k,
					 scalar x, // vector
					 scalar y,
					 scalar z);
  void free_default_motion_state(motion_state* motion_state);
  transform* get_world_transform(motion_state* motion_state);

  // btRigidBody
  rigid_body* new_rigid_body(scalar mass,
			     motion_state* motion_state,
			     collision_shape* shape,
			     scalar inertia_x,
			     scalar inertia_y,
			     scalar inertia_z);
  void free_rigid_body(rigid_body* rigid_body);
  motion_state* get_motion_state(rigid_body* rigid_body);

  // btCollisionShape
  void calculate_local_inertia(collision_shape* collision_shape,
			       scalar mass,
			       scalar* x_out,
			       scalar* y_out,
			       scalar* z_out);

  // btStaticPlaneShape
  static_plane_shape* new_static_plane_shape(scalar x,
					     scalar y,
					     scalar z,
					     scalar plane_constant);
  void free_static_plane_shape(static_plane_shape* static_plane_shape);

  // btSphereShape
  sphere_shape* new_sphere_shape(scalar radius);
  void free_sphere_shape(sphere_shape* sphere_shape);

  // btTransform
  void get_origin(transform* transform, scalar* x_out, scalar* y_out, scalar* z_out);
  
#ifdef __cplusplus
}
#endif

#endif
